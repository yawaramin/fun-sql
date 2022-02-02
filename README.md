## ocaml_sql_query - functional-style SQLite queries for OCaml

Copyright 2022 Yawar Amin

This file is part of ocaml_sql_query.

ocaml_sql_query is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

ocaml_sql_query is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
ocaml_sql_query. If not, see <https://www.gnu.org/licenses/>.

### Examples

```ocaml
open Sqlite3
open Sql

(* Test DB *)
let db = db_open ":memory:"
(* val db : Sqlite3.db = <abstr> *)

(* DDL query with no arguments and no return *)
let _ = query
  db
  "create table people (name text not null, age int not null)"
  ret_unit

(* Insert query with single row *)
let _ = query
  db
  "insert into people (name, age) values (?, ?)"
  (text "A")
  (int 1)
  ret_unit

(* Get text value from DB *)
let name_1 = query db "select name from people where rowid = ?" (int 1) ret_text
(* val name_1 : string Seq.t = <fun> *)

(* Map return data to a custom type on the fly *)
type person = { name : string; age : int }

let ret_person = ret @@ function
  | [|Data.TEXT name; INT age|] -> Ok { name; age = Int64.to_int age }
  | _ -> Error "malformed data"
(* val ret_person : (person Seq.t, int) Sql.t = <abstr> *)

(* Assert resultset has a single row and map it *)
let person_1 = only
  @@ query db "select name, age from people where rowid = ?" (int 1) ret_person
(* val person_1 : person = {name = "A"; age = 1} *)
```

### Batch insert

Currently investigating the design.
