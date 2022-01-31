## ocaml_sql_query - functional-style SQLite queries for OCaml

### Examples

```ocaml
open Sqlite3
open Sql

type person = { name : string; age : int }
let person_encoder { name; age } = [Data.TEXT name; INT (Int64.of_int age)]

(* Test DB *)
let db = db_open ":memory:"

(* DDL query with no arguments and no return *)
let _ = query
  db
  "create table people (name text not null, age int not null)"
  ret_unit

(* Insert query with
let _ = insert
  db
  "insert into people (name, age) values (?, ?)"
  person_encoder
  [{ name = "A"; age = 1 }; { name = "B"; age = 2 }]
  ret_unit

let names = query db "select name from people where rowid = ?" (int 1) ret_text
```
