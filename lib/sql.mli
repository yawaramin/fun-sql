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

type ('a, 's) t
(** A query component that can be composed with query parameters and return
    types. *)

type 'a query = ('a, int) t -> 'a

type 'a param = ('a query, int) t
(** A query parameter. The function with [int]s represents that the parameter is
    positionally bound, and the position index incremented when that happens.
    The user doesn't need to manually track the numbers and positions of
    bindings, they just need to provide the corresponding binding values for any
    positional variables in the SQL query, represented by [?]. *)

(** {2 Query runners} *)

val query : db -> string -> 'a query
(** [query db sql ...] executes a query [sql] against the database [db]. The
    arguments to be bound to the query, if any, are passed in following [sql].
    The return type of the query is then passed in as the final argument. *)

val batch_insert :
  db ->
  string ->
  'a list ->
  ('b query -> 'a -> 'b query) ->
  'b query
(** [insert db sql objs args] inserts into the database [db], running
    the query [sql], the row tuples obtained by encoding the list of [objs]
    using the [args] parameters and return type encodings.

    The return type of the query is passed in as [ret_type]. If the query
    doesn't return anything, i.e. it doesn't have a [returning] clause,
    [ret_unit] should be passed in. *)

(** {2 Query binding arguments} *)

val text : string -> _ param
val bool : bool -> _ param
val int : int -> _ param
val nativeint : nativeint -> _ param
val int32 : int32 -> _ param
val int64 : int64 -> _ param
val double : float -> _ param
val blob : string -> _ param

(** {2 Query return types} *)

val ret : (Data.t array -> ('a, string) result) -> ('a Seq.t, _) t
(** [ret decoder] is a custom return type encoding for a resultset into a
    sequence of values of the type decoded by [decoder].

    @raise Invalid_argument if any row cannot be decoded.
    @raise Failure if an unexpected result code is encountered. *)

val ret_unit : ((unit, string) result, _) t
val ret_int64 : (int64 Seq.t, _) t
val ret_float : (float Seq.t, _) t
val ret_text : (string Seq.t, _) t
val ret_blob : (string Seq.t, _) t
