(* Copyright 2022 Yawar Amin

   This file is part of ocaml_sql_query.

   ocaml_sql_query is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option) any
   later version.

   ocaml_sql_query is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   ocaml_sql_query. If not, see <https://www.gnu.org/licenses/>. *)

type db = Sqlite3.db
type arg = Sqlite3.Data.t
type row = Sqlite3.Data.t array
type 'r ret = Unit : unit ret | Ret : (row -> 'r) -> 'r Seq.t ret

open Sqlite3

let check_rc = function
  | Rc.OK
  | DONE -> ()
  | rc -> failwith (Rc.to_string rc)

let query : type r. db -> string -> ?args:arg list -> r ret -> r =
  fun db sql ->
  let stmt = prepare db sql in
  fun ?args ->
  ignore @@ reset stmt;
  Option.iter (fun arg -> check_rc @@ bind_values stmt arg) args;
  function
  | Unit ->
    check_rc @@ step stmt;
    check_rc @@ reset stmt
  | Ret decode ->
    let rows () = match step stmt with
      | ROW ->
        Some (stmt |> row_data |> decode, ())
      | DONE ->
        None
      | rc ->
        failwith @@ Rc.to_string rc
    in
    Seq.unfold rows ()

let rec exec_script db attempts stmt = match step stmt, attempts with
  | Rc.BUSY, 0 ->
    failwith "busy"
  | BUSY, _ ->
    exec_script db (pred attempts) stmt
  | ROW, _ ->
    exec_script db attempts stmt
  | (DONE | OK), _ ->
    check_rc @@ finalize stmt;
    begin match prepare_tail stmt with
    | Some stmt -> exec_script db attempts stmt
    | None -> ()
    end
  | rc, _ ->
    invalid_arg @@ Rc.to_string rc

let exec_script db sql = exec_script db 3 @@ prepare db sql

module Arg = struct
  let text value = Data.TEXT value
  let bool value = Data.INT (if value then 1L else 0L)
  let int value = Data.INT (Int64.of_int value)
  let nativeint value = Data.INT (Int64.of_nativeint value)
  let int32 value = Data.INT (Int64.of_int32 value)
  let int64 value = Data.INT value
  let float value = Data.FLOAT value
  let blob value = Data.TEXT value

  let opt data = function
    | Some value -> data value
    | None -> Data.NULL
end

let unit = Unit
let ret decode = Ret decode

let int64 pos row = match row.(pos) with
  | Data.INT value -> value
  | _ -> failwith "Expected int"

let int pos row = Int64.to_int @@ int64 pos row

let bool pos row = Int64.compare (int64 pos row) Int64.zero > 0

let float pos row = match row.(pos) with
  | Data.FLOAT value -> value
  | _ -> failwith "Expected float"

let text pos row = match row.(pos) with
  | Data.INT value -> Int64.to_string value
  | FLOAT value -> string_of_float value
  | BLOB value
  | TEXT value -> value
  | _ -> failwith "Expected text"

let opt dec col row = match row.(col) with
  | Data.NULL -> None
  | _ -> Some (dec col row)

let only_fail typ =
  let msg = if typ then "more than one" else "none" in
  failwith ("Seq.only: require exactly one item, found " ^ msg)

let only seq = match seq () with
  | Seq.Nil ->
    only_fail false
  | Cons (a, seq) ->
    match seq () with
    | Nil -> a
    | Cons (_, _) -> only_fail true

let optional seq = match seq () with
  | Seq.Nil ->
    None
  | Cons (a, seq) ->
    match seq () with
    | Nil -> Some a
    | Cons (_, _) -> only_fail true

let transaction db f =
  query db "begin" unit;
  match f () with
  | r ->
    query db "commit" unit;
    r
  | exception e ->
    query db "rollback" unit;
    raise e

(* We have to parse the value tuple of the insert statement to be able to
   multiply it a number of times if needed for a batch insert. *)

let sp_r = Str.regexp " +"
let wsp_r = Str.regexp
  @@ Printf.sprintf "[%c%c%c]+" (Char.chr 9) (Char.chr 10) (Char.chr 13)
let values_r = Str.regexp_case_fold
  {|^\(insert into .* values *\)\(([^)]+)\)\(.*\)$|}

let pre_pos = 1
let tuple_pos = 2
let post_pos = 3

let batch_insert db sql objs obj_args =
  let sql = sql
    |> String.trim
    |> Str.global_replace wsp_r " "
    |> Str.global_replace sp_r " "
  in
  if Str.string_match values_r sql 0 then
    let tuple = Str.matched_group tuple_pos sql in
    let tuples = objs |> List.map (fun _ -> tuple) |> String.concat "," in
    let sql =
      Str.matched_group pre_pos sql ^ tuples ^ Str.matched_group post_pos sql
    in
    query db sql ~args:(objs |> List.map obj_args |> List.flatten)
  else
    failwith sql

let slurp file =
  let inc = open_in file in
  let contents = really_input_string inc (in_channel_length inc) in
  close_in inc;
  contents

exception Bad_migration of string

let migrate db dir =
  query db
    "
    create table if not exists migration (
      filename varchar(1024) not null primary key,
      script text not null,
      applied_at timestamp
    )
    "
    unit;
  let mark_ok = query db
    "
    insert into migration (filename, script, applied_at)
    values (?, ?, current_timestamp)
    "
  in
  let migrated = query db "select 1 from migration where filename = ?" in
  let migrated filename = 0
    |> bool
    |> ret
    |> migrated ~args:[filename]
    |> optional
    |> Option.fold ~none:false ~some:Fun.id
  in
  let files = Sys.readdir dir in
  Array.sort compare files;
  transaction db @@ fun () ->
  files |> Array.iter @@ fun filename ->
    let filename = dir ^ "/" ^ filename in
    let arg_filename = Arg.text filename in
    if String.ends_with ~suffix:".sql" filename && not (migrated arg_filename) then
      let script = slurp filename in
      match exec_script db script with
      | () ->
        mark_ok ~args:Arg.[arg_filename; text script] unit
      | exception Failure _ ->
        raise (Bad_migration (Sqlite3.errmsg db))
