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

type db = Sqlite3.db
type arg = Sqlite3.Data.t
type _ ret

(** {2 Query runners} *)

val query : db -> string -> ?args:arg list -> 'r ret -> 'r
(** [query db sql ?args ret] executes a query [sql] against the database [db].
    The arguments to be bound to the query, if any, are passed in [args]. The
    return value of the query, if any, is decoded by [ret].

    Note, this function is designed so you can prepare each SQL statement only
    once, and run it each time with different parameters if needed. This may
    give you an efficiency boost over preparing the statement each time:

    {[ let create_user = query db "insert into users (name, age) values (?, ?)"
       let create_user name age = create_user ~args:Arg.[text name; int age] unit ]} *)

val batch_insert : db -> string -> 'a list -> ('a -> arg list) -> 'r ret -> 'r
(** [batch_insert db sql objs obj_args ret] inserts into the database [db],
    running the query [sql], the row tuples obtained by encoding the list of
    [objs] using the [obj_args] function.

    This prepares a new statement each time because the [VALUES (...)] clause
    may contain different numbers of placeholders in each call.

    The return type of the query is decoded by [ret]. *)

(** {2 Binding arguments}

    These encode OCaml data as data to be bound to the query statement. *)

module Arg : sig
  val text : string -> arg
  val bool : bool -> arg
  val int : int -> arg
  val nativeint : nativeint -> arg
  val int32 : int32 -> arg
  val int64 : int64 -> arg
  val float : float -> arg
  val blob : string -> arg

  val opt : ('a -> arg) -> 'a option -> arg
  (** [opt data value] is the optional [value] encoded as query data. *)
end

(** {2 Return types} *)

type row = Sqlite3.Data.t array

val unit : unit ret
(** [unit] indicates that the query doesn't return any meaningful output. *)

val ret : (row -> 'a) -> 'a Seq.t ret
(** [ret decode] is a custom return type encoding for a resultset into a
    sequence of values of the type decoded by [decode].

    [decode] constructs a value of the custom type if possible, else raises
    [Failure].

    Note that the sequence rows of the resultset is unfolded as it is read from
    the database. It can only be traversed {i once,} with e.g. [List.of_seq] or
    [Seq.iter]. If traversed multiple times, it will raise [Failure].

    @raise Invalid_argument if any row cannot be decoded.
    @raise Failure if an unexpected result code is encountered. *)

(** {3 Helpers to get typed values from columns} *)

val int : int -> row -> int
val int64 : int -> row -> int64
val float : int -> row -> float

val text : int -> row -> string
(** Also handles values of all other types. Use this when SQLite can change the
    exact type of value it returns at runtime, e.g. for very large numbers it
    can return text. *)

val opt : (int -> row -> 'a) -> int -> row -> 'a option
(** [opt dec col row] is the optional value [NULL] turns to [None] at column
    [col] of the result [row]. *)

val only : 'a Seq.t -> 'a
(** [only seq] is the first and only element of [seq]. This is a convenience
    function because all queries return seqs but sometimes we want only a single
    item, otherwise it should be an error.

    Use this in preference to calculating the length of the [seq], which would
    force the entire data structure.

    @raise Failure if seq has 0 or >1 items. *)
