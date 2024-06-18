## fun-sql - functional-style SQL queries for OCaml

Copyright 2022 Yawar Amin

This file is part of fun-sql.

fun-sql is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

fun-sql is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with fun-
sql. If not, see <https://www.gnu.org/licenses/>.

### What is this

A simple functional-style query runner and mapper for PostgreSQL and SQLite
(MySQL support is also desired if anyone wants to contribute!).

Designed to make it easy to create prepared statements and then use them:

```ocaml
(* Prepared statement: *)
let edit_note = query db "update note set txt = $1 where id = $2"

(* Use by simply calling it: *)
let edit_note id txt = edit_note ~args:Arg.[int id; text txt] unit
```

### Examples

```ocaml
open Fun_postgresql
(* Or: Fun_sqlite *)

(* Test DB *)
let db = new Postgresql.connection ~conninfo:"postgresql://..." ()
(* Or: let db = Sqlite3.db_open ":memory:" *)

(* DDL query with no arguments and no return *)
let () = query db "create table people (name text not null, age int)" unit

(* Insert query with single row *)
let () = query
  db
  (sql "insert into people (name, age) values (%a, %a)" placeholder 0 placeholder 1)
  ~args:Arg.[text "A"; int 1]
  unit

(* Get single column of results from DB *)
let names = query
  db
  (sql "select name from people where age = %a" placeholder 0)
  ~args:Arg.[int 1]
  @@ ret @@ text 0
(* val names : string Seq.t = <fun> *)

let () =
  query
    db
    (sql "insert into people (name) values (%a)" placeholder 0)
    ~args:Arg.[text "B"]
    unit

(* Get optional values *)
let ages = List.of_seq
  @@ query db "select age from people" @@ ret @@ opt int 0
(* val ages : int option list = [Some 1; None] *)

(* Map return data to a custom type on the fly *)
type person = { name : string; age : int option }

let person row = { name = text 0 row; age = opt int 1 row }
(* val person : row -> person *)

(* Assert resultset has a single row and map it *)
let person_1 = only
  @@ query db (sql "select name, age from people where age = %a" placeholder 0) ~args:Arg.[int 1]
  @@ ret person
(* val person_1 : person = {name = "A"; age = Some 1} *)

(* Assert resultset has either 0 or 1 element *)
let opt_person_1 = optional
  @@ query db (sql "select name, age from people where age = %a" placeholder 0) ~args:Arg.[int 2]
  @@ ret person
(* val opt_person_1 : person option = None *)

(* Batch insert *)

let ppl = [{ name = "B"; age = None }; { name = "C"; age = Some 3 }]

(* Batch insert works with SQLite only: *)
let () = batch_insert
  db
  (sql "insert into people (name, age) values (%a, %a)" placeholder 0 placeholder 1)
  ppl
  (fun { name; age } -> Arg.[text name; opt int age])
  unit
```

### PPX

There is basic support for deriving resultset row decoders:

```
# open Fun_sqlite;;
# let db = Sqlite3.db_open "fun.db";;
val db : db = <abstr>
# module Person = struct
  type t = { name : string; age : int option } [@@deriving funsql]
end;;
module Person :
  sig type t = { name : string; age : int option; } val t : row -> t end
# List.of_seq (query db "select name, age from people" (ret Person.t));;
- : Person.t list =
[{Person.name = "Tim"; age = Some 36}; {Person.name = "Bob"; age = Some 32};
 {Person.name = "Foo"; age = Some 1}; {Person.name = "X"; age = Some 22};
 {Person.name = "N"; age = Some 55}]
```

To use the PPX, add to your `dune` file:

```
(preprocess (pps ppx_deriving_funsql))
```

And to your `dune-project` file:

```
(depends
  ...
  ppx_deriving_funsql
  ...)
```
