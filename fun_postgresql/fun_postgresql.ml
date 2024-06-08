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

type db = Postgresql.connection
type arg = string
type row = string array

type 'r ret =
  | Unit : unit ret
  | Ret : (row -> 'r) -> 'r Seq.t ret

type 'a dql = ?args:arg list -> 'a Seq.t ret -> 'a Seq.t
type dml = ?args:arg list -> unit ret -> unit

exception More_than_one

let query : type r. db -> string -> ?args:arg list -> r ret -> r =
 fun db sql ->
  let stm_name = Digest.(to_hex (string sql)) in
  let result = db#prepare stm_name sql in
  match result#status with
  | Postgresql.Bad_response | Nonfatal_error -> failwith result#error
  | Fatal_error -> invalid_arg result#error
  | _ -> (
    fun ?args ->
      let params = Option.map Array.of_list args in
      let result = db#exec_prepared ?params stm_name in
      match result#status with
      | Postgresql.Tuples_ok | Command_ok | Single_tuple -> (
        function
        | Unit -> ()
        | Ret decode ->
          Seq.init result#ntuples (fun i -> decode (result#get_tuple i)))
      | _ -> failwith result#error)

let exec_script (db : db) sql =
  let result = db#exec sql in
  match result#status with
  | Postgresql.Bad_response | Nonfatal_error -> failwith result#error
  | Fatal_error -> invalid_arg result#error
  | _ -> ()

module Arg = struct
  let text value = value
  let bool = string_of_bool
  let int = string_of_int
  let nativeint = Nativeint.to_string
  let int32 = Int32.to_string
  let int64 = Int64.to_string
  let float value = Printf.sprintf "%f" value
  let blob value = value

  let opt data = function
    | Some value -> data value
    | None -> Postgresql.null
end

let unit = Unit
let ret decode = Ret decode
let int64 pos row = Int64.of_string row.(pos)
let int pos row = int_of_string row.(pos)
let bool pos row = bool_of_string row.(pos)
let float pos row = float_of_string row.(pos)
let text pos row = row.(pos)

let opt dec col row =
  if row.(col) = Postgresql.null then None else Some (dec col row)

let only seq =
  match seq () with
  | Seq.Nil -> raise Not_found
  | Cons (a, seq) -> (
    match seq () with
    | Nil -> a
    | Cons (_, _) -> raise More_than_one)

let optional seq =
  match seq () with
  | Seq.Nil -> None
  | Cons (a, seq) -> (
    match seq () with
    | Nil -> Some a
    | Cons (_, _) -> raise More_than_one)

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

let wsp_r =
  Str.regexp
  @@ Printf.sprintf "[%c%c%c]+" (Char.chr 9) (Char.chr 10) (Char.chr 13)

let values_r =
  Str.regexp_case_fold {|^\(insert into .* values *\)\(([^)]+)\)\(.*\)$|}

let pre_pos = 1
let tuple_pos = 2
let post_pos = 3

let batch_insert db sql objs obj_args =
  let sql =
    sql
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
  exec_script db
    "create table if not exists migration (
      filename varchar(1024) not null primary key,
      script text not null,
      applied_at timestamp
    )";
  let mark_ok =
    query db
      "insert into migration (filename, script, applied_at)
       values ($1, $2, current_timestamp)"
  in
  let migrated = query db "select 1 from migration where filename = $1" in
  let migrated filename =
    0
    |> bool
    |> ret
    |> migrated ~args:[filename]
    |> optional
    |> Option.fold ~none:false ~some:Fun.id
  in
  let files = Sys.readdir dir in
  Array.sort compare files;
  transaction db @@ fun () ->
  files
  |> Array.iter @@ fun filename ->
     let filename = dir ^ "/" ^ filename in
     let arg_filename = Arg.text filename in
     if String.ends_with ~suffix:".sql" filename && not (migrated arg_filename)
     then
       let script = slurp filename in
       match exec_script db script with
       | () -> mark_ok ~args:Arg.[arg_filename; text script] unit
       | exception Failure msg -> raise (Bad_migration msg)
