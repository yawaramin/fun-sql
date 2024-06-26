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
let edit_note = query db "update note set txt = ? where id = $2"

(* Use by simply calling it: *)
let edit_note id txt = edit_note Arg.[int id; text txt] Fun_sql.unit
```

### Examples

```ocaml
open Fun_postgresql
(* Or: Fun_sqlite *)

(* Test DB *)
let db = new Postgresql.connection ~conninfo:"postgresql://..." ()
(* Or: let db = Sqlite3.db_open ":memory:" *)

(* DDL query with no arguments and no return *)
let () = query db "create table people (name text not null, age int)" [] unit

(* Insert query with single row *)
let () = query
  db
  (sql "insert into people (name, age) values (%a, %a)" placeholder 0 placeholder 1)
  Arg.[text "A"; int 1]
  unit

(* Get single column of results from DB *)
let names = query
  db
  (sql "select name from people where age = %a" placeholder 0)
  Arg.[int 1]
  (ret (text 0))
(* val names : string Seq.t = <fun> *)

let () =
  query
    db
    (sql "insert into people (name) values (%a)" placeholder 0)
    Arg.[text "B"]
    unit

(* Get optional values *)
let ages = all (query db "select age from people" [] (ret (opt int 0)))
(* val ages : int option list = [Some 1; None] *)

(* Map return data to a custom type on the fly–see also below for a PPX deriver
   that automates this boilerplate *)
type person = { name : string; age : int option }

let ret = ret (fun row -> { name = text 0 row; age = opt int 1 row })
(* val ret : (row, person Seq.t) ret *)

(* Assert resultset has at most a single row and map it *)
let person_1 =
  one (
    query
      db
      (sql "select name, age from people where age = %a" placeholder 0)
      Arg.[int 1]
      ret)
(* val person_1 : person option = Some {name = "A"; age = Some 1} *)

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

There is support for deriving resultset row decoders:

> [!NOTE]
> One of the query modules _must_ be `open`ed for the PPX to work, ie either
> `Fun_sqlite` or `Fun_postgresql`. You can avoid polluting your scope by putting
> the `open`s inside submodules.

To use the PPX, add to your `dune` file, eg:

```
(executable
  ...
  (preprocess (pps ppx_deriving_funsql)))
```

And to your `dune-project` file:

```
(package
  ...
  (depends
    ...
    ppx_deriving_funsql
    ...))
```

### Deriving for custom types with the PPX

The PPX supports the following basic types:

- `int`
- `string`
- `float`
- `int64`
- `bool`
- `t option` where `t` is a type which can be derived. This can be used to
  represent a nullable type.

It also supports custom types as long as the type is in a module which conforms
to the signature:

```ocaml
sig
  type t
  val of_string : string -> t
end
```

In other words it uses the module to 'find' the parser of the type from its
string representation. Eg:

```
# #require "decimal";;
# #install_printer Decimal.pp;;
# let db = Sqlite3.db_open "fun.db";;
val db : Sqlite3.db = <abstr>
# module Person = struct
  open Fun_sqlite

  type t = { name : string; age : Decimal.t option } [@@deriving funsql]

  let by_name = query db (sql "select name, age from people where name = %a" placeholder 0)
  let by_name name = by_name Arg.[text name] ret
end;;
module Person :
  sig
    type t = { name : string; age : Decimal.t option; }
    val ret : (row, t Seq.t) ret
    val by_name : string -> t Seq.t
  end
# all (Person.by_name "Bob");;
- : Person.t list = [{Person.name = "Bob"; age = Some 32}]
# one (Person.by_name "X");;
- : Person.t option = Some {Person.name = "X"; age = Some 22}
```

> [!WARNING]
> The record type fields must be declared in the same order as the `select`
> clause fields in the SQL query, as the deriver relies on this ordering.

I believe this is not a huge problem though, because fortunately OCaml makes it
easy to create small, dedicated submodules for each query as needed.
