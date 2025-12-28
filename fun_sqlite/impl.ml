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

let placeholder f _ = Format.pp_print_char f '?'

open Sqlite3

type nonrec db = db
type arg = Data.t
type row = Data.t array

include Fun_sql.Query

let check_rc = function
  | Rc.OK | DONE -> ()
  | rc -> failwith (Rc.to_string rc)

let query : type r. db -> string -> arg list -> (row, r) ret -> r =
 fun db sql ->
  let stmt = prepare db sql in
  fun args ->
    ignore @@ reset stmt;
    (match args with
    | [] -> ()
    | _ -> check_rc @@ bind_values stmt args);
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

let rec exec_script db attempts sql =
  match exec db sql, attempts with
  | Rc.BUSY, 0 -> failwith "busy"
  | BUSY, _ -> exec_script db (pred attempts) sql
  | ROW, _ -> exec_script db attempts sql
  | (DONE | OK), _ -> ()
  | rc, _ -> rc |> Rc.to_string |> invalid_arg

let exec_script db = exec_script db 3

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
