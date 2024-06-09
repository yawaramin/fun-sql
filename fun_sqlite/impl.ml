(* Copyright 2022 Yawar Amin

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

type db = Sqlite3.db
type arg = Sqlite3.Data.t
type row = Sqlite3.Data.t array

type 'r ret =
  | Unit : unit ret
  | Ret : (row -> 'r) -> 'r Seq.t ret

let placeholder _ = "?"

open Sqlite3

let check_rc = function
  | Rc.OK | DONE -> ()
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
      let rows () =
        match step stmt with
        | ROW -> Some (stmt |> row_data |> decode, ())
        | DONE -> None
        | rc -> failwith @@ Rc.to_string rc
      in
      Seq.unfold rows ()

let rec exec_script db attempts stmt =
  match step stmt, attempts with
  | Rc.BUSY, 0 -> failwith "busy"
  | BUSY, _ -> exec_script db (pred attempts) stmt
  | ROW, _ -> exec_script db attempts stmt
  | (DONE | OK), _ ->
    check_rc @@ finalize stmt;
    begin
      match prepare_tail stmt with
      | Some stmt -> exec_script db attempts stmt
      | None -> ()
    end
  | rc, _ -> invalid_arg @@ Rc.to_string rc

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

let int64 pos row =
  match row.(pos) with
  | Data.INT value -> value
  | _ -> failwith "Expected int"

let int pos row = Int64.to_int @@ int64 pos row
let bool pos row = Int64.compare (int64 pos row) Int64.zero > 0

let float pos row =
  match row.(pos) with
  | Data.FLOAT value -> value
  | _ -> failwith "Expected float"

let text pos row =
  match row.(pos) with
  | Data.INT value -> Int64.to_string value
  | FLOAT value -> string_of_float value
  | BLOB value | TEXT value -> value
  | _ -> failwith "Expected text"

let opt dec col row =
  match row.(col) with
  | Data.NULL -> None
  | _ -> Some (dec col row)
