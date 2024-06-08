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

type db = Postgresql.connection
type arg
type _ ret

(** {2 Convenience types for prepared statements} *)

type 'a dql = ?args:arg list -> 'a Seq.t ret -> 'a Seq.t
(** Represents a Data Query Language query. Note, update statements can also
    return results nowadays so technically this naming is slightly inaccurate. *)

type dml = ?args:arg list -> unit ret -> unit
(** Represents a Data Modification Language query. *)

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

val exec_script : db -> string -> unit
(** [exec_script db sql] executes the [sql] script (possibly made up of multiple
    statements) in the database [db]. Note that it ignores any rows returned by
    any of the statements.

    The script {i must not} have a trailing semicolon. *)

exception Bad_migration of string

val migrate : db -> string -> unit
(** [migrate db dir] applies the SQL migration scripts in [dir] on the given
    database [db], keeping track of those that have already been applied.

    To apply the migrations in the correct order, the migration scripts must be
    given filenames that are sorted in lexicographical order of the desired
    migration order, e.g. [0000_0001_init.sql] will be applied before
    [0000_0002_sec.sql], and so on.

    Note that this uses [exec_script] internally, which means the migration
    scripts {i must not} have trailing semicolons either.

    Any files with extensions other than [.sql] are ignored.

    @raise Bad_migration an error occurs during applying the migrations. *)

val transaction : db -> (unit -> 'r) -> 'r
(** [transaction db f] runs [f ()] inside a transaction in the [db]. If the
    operation succeeds, it commits the transaction and returns its result. If it
    fails with an exception, it rolls back the transaction and re-raises the
    exception. *)

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

type row = string array

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
val bool : int -> row -> bool
val int64 : int -> row -> int64
val float : int -> row -> float

val text : int -> row -> string
(** Also handles values of all other types. *)

val opt : (int -> row -> 'a) -> int -> row -> 'a option
(** [opt dec col row] is the optional value [NULL] turns to [None] at column
    [col] of the result [row]. *)

(** {3 Helpers to deal with resultset sequences} *)

exception More_than_one
(** Thrown if we are expecting at most one result but get more. *)

val only : 'a Seq.t -> 'a
(** [only seq] is the first and only element of [seq]. This is a convenience
    function because all queries return seqs but sometimes we want only a single
    item, otherwise it should be an error.

    Use this in preference to calculating the length of the [seq], which would
    force the entire data structure.

    @raise Not_found if [seq] has 0 items.
    @raise More_than_one if [seq] has more than 1 item. *)

val optional : 'a Seq.t -> 'a option
(** [optional seq] is [Some a] if [a] is the first and only element of [seq], or
    [None] if [seq] is empty.

    @raise More_than_one if [seq] has more than 1 item. *)
