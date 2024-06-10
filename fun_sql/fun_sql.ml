(** This module is used for common code across different SQL database engines.
    You normally will not be using this module, instead you would directly use
    either [Fun_sqlite] or [Fun_postgresql]. *)

module type Sql = sig
  type db
  type arg
  type _ ret

  val placeholder : int -> string
  (** A generic way to distinguish between placeholders for different database
      engines' prepared statement parameters.

      Note: placeholders are 0-indexed. *)

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

  val exec_script : db -> string -> unit
  (** [exec_script db sql] executes the [sql] script (possibly made up of multiple
    statements) in the database [db]. Note that it ignores any rows returned by
    any of the statements.

    The script {i must not} have a trailing semicolon. *)

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

  type row

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
  (** Also handles values of all other types. Use this when SQLite can change the
    exact type of value it returns at runtime, e.g. for very large numbers it
    can return text. *)

  val opt : (int -> row -> 'a) -> int -> row -> 'a option
  (** [opt dec col row] is the optional value [NULL] turns to [None] at column
    [col] of the result [row]. *)
end

module type S = sig
  type db
  type arg
  type _ ret

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
end

module Make (Sql : Sql) :
  S with type db = Sql.db and type arg = Sql.arg and type 'a ret = 'a Sql.ret =
struct
  type db = Sql.db
  type arg = Sql.arg
  type 'a ret = 'a Sql.ret

  exception More_than_one

  let only seq =
    match seq () with
    | Seq.Nil -> raise Not_found
    | Cons (a, seq) -> (
      match seq () with
      | Nil -> a
      | Cons (_, _) -> raise More_than_one)

  let optional seq =
    match seq () with
    | Seq.Nil -> None
    | Cons (a, seq) -> (
      match seq () with
      | Nil -> Some a
      | Cons (_, _) -> raise More_than_one)

  open Sql

  let transaction db f =
    query db "begin" unit;
    match f () with
    | r ->
      query db "commit" unit;
      r
    | exception e ->
      query db "rollback" unit;
      raise e

  let slurp file =
    let inc = open_in file in
    Fun.protect
      ~finally:(fun () -> close_in inc)
      (fun () -> really_input_string inc (in_channel_length inc))

  exception Bad_migration of string

  let spr = Printf.sprintf

  let migrate db =
    exec_script db
      "create table if not exists migration (
         filename varchar(1024) not null primary key,
         script text not null,
        applied_at timestamp
       )";
    let mark_ok =
      query db
        (spr
           "insert into migration (filename, script, applied_at)
         values (%s, %s, current_timestamp)"
           (placeholder 0) (placeholder 1))
    in
    let migrated =
      query db
        (spr "select 1 from migration where filename = %s" (placeholder 0))
    in
    let migrated filename =
      0
      |> bool
      |> ret
      |> migrated ~args:[filename]
      |> optional
      |> Option.fold ~none:false ~some:Fun.id
    in
    fun dir ->
      let files = Sys.readdir dir in
      Array.sort compare files;
      transaction db @@ fun () ->
      files
      |> Array.iter @@ fun filename ->
         let filename = dir ^ "/" ^ filename in
         let arg_filename = Arg.text filename in
         if
           String.ends_with ~suffix:".sql" filename
           && not (migrated arg_filename)
         then
           let script = slurp filename in
           match exec_script db script with
           | () -> mark_ok ~args:Arg.[arg_filename; text script] unit
           | exception Failure msg -> raise (Bad_migration msg)
end
