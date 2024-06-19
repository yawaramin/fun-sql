(* Copyright 2024 Yawar Amin

   This file is part of fun-sql.

   fun-sql is free software: you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   fun-sql is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   fun-sql. If not, see <https://www.gnu.org/licenses/>. *)

type db = Postgresql.connection
type arg = string
type row = string array

let placeholder f n = Format.fprintf f "$%d" (n + 1)

let query : type r. db -> string -> arg list -> (row, r) Fun_sql.ret -> r =
 fun db sql ->
  let stm_name = Digest.(to_hex (string sql)) in
  let result = db#prepare stm_name sql in
  match result#status with
  | Postgresql.Bad_response | Nonfatal_error -> failwith result#error
  | Fatal_error -> invalid_arg result#error
  | _ -> (
    fun args ->
      let params =
        match args with
        | [] -> None
        | _ -> Some (Array.of_list args)
      in
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

let unit = Fun_sql.Unit
let ret decode = Fun_sql.Ret decode
let int64 pos row = Int64.of_string row.(pos)
let int pos row = int_of_string row.(pos)
let bool pos row = bool_of_string row.(pos)
let float pos row = float_of_string row.(pos)
let text pos row = row.(pos)

let opt dec col row =
  if row.(col) = Postgresql.null then None else Some (dec col row)
