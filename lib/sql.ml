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

open Sqlite3

type ('a, 's) t = stmt -> 's -> 'a
type 'a query = ('a, int) t -> 'a
type 'a param = ('a query, int) t
type row = Data.t array

let query db sql =
  let stmt = prepare db sql in
  fun k -> k stmt 1

let param typ stmt pos value =
  ignore (typ stmt pos value);
  fun k -> k stmt (succ pos)

let text value stmt pos = param bind_text stmt pos value
let bool value stmt pos = param bind_bool stmt pos value
let int value stmt pos = param bind_int stmt pos value
let nativeint value stmt pos = param bind_nativeint stmt pos value
let int32 value stmt pos = param bind_int32 stmt pos value
let int64 value stmt pos = param bind_int64 stmt pos value
let double value stmt pos = param bind_double stmt pos value
let blob value stmt pos = param bind_blob stmt pos value

let ret decode stmt _ =
  let rows () = match step stmt with
    | ROW ->
      Some (stmt |> row_data |> decode, ())
    | DONE ->
      None
    | rc ->
      let rc_str = Rc.to_string rc in
      failwith ("Sql.rows: invalid operation: " ^ rc_str)
  in
  let seq = Seq.unfold rows () in
  ignore (reset stmt);
  seq

let check_rc = function
  | Rc.OK
  | DONE -> ()
  | rc -> failwith (Rc.to_string rc)

let unit stmt _ =
  check_rc @@ step stmt;
  check_rc @@ reset stmt

let int64' pos row = match row.(pos) with
  | Data.INT value -> value
  | _ -> failwith "Expected int"

let int' pos row = Int64.to_int @@ int64' pos row

let float pos row = match row.(pos) with
  | Data.FLOAT value -> value
  | _ -> failwith "Expected float"

let text' pos row = match row.(pos) with
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

let batch_insert db sql objs =
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
    fun k -> List.fold_left k (query db sql) objs
  else
    failwith ("insert: expected valid statement, got '" ^ sql ^ "'")
