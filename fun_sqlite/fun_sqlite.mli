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

(** Use this module for SQLite database queries. *)

include
  Fun_sql.Sql with type db = Sqlite3.db and type row = Sqlite3.Data.t array

include Fun_sql.S with type db := db and type arg := arg

val batch_insert :
  db -> string -> 'a list -> ('a -> arg list) -> (row, 'r) Fun_sql.ret -> 'r
(** [batch_insert db sql objs obj_args ret] inserts into the database [db],
     running the query [sql], the row tuples obtained by encoding the list of
     [objs] using the [obj_args] function.

     This prepares a new statement each time because the [VALUES (...)] clause
     may contain different numbers of placeholders in each call.

     The return type of the query is decoded by [ret]. *)
