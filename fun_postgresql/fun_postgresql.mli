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

(** Use this module for PostgreSQL database queries.

    Note: does not support [batch_insert] out of the box because PostgreSQL uses
    numbered placeholders, which makes it much more difficult to parse out and
    modify the insert query with the correct number of placeholders. *)

include Fun_sql.Sql with type db = Postgresql.connection

include
  Fun_sql.S with type db := db and type arg := arg and type 'a ret := 'a ret
