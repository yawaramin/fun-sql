(** This module is used for common code across different SQL database engines.
    You normally will not be using this module, instead you would directly use
    either [Fun_sqlite] or [Fun_postgresql]. *)

(** A decoder of a single row of the resultset from running a query. *)
type (-'row, 'r) ret =
  | Unit : ('row, unit) ret
      (** This is used for queries which do not return result rows. *)
  | Ret : ('row -> 'r) -> ('row, 'r Seq.t) ret
      (** This is for queries which return result rows. *)

module type Sql = sig
  type db
  (** The database connection or file, etc. *)

  type arg
  (** A value sent to the database in the place of a query parameter. *)

  type row
  (** A tuple from the database. *)

  val placeholder : Format.formatter -> int -> unit
  (** A generic way to write placeholders for different database drivers'
      prepared statement parameters.

      ℹ️ Placeholders are 0-indexed. *)

  (** {2 Query runners} *)

  val query : db -> string -> arg list -> (row, 'r) ret -> 'r
  (** The main function through which queries are run is the [query] function.
      This function {e always} creates a prepared statement for each partial call
      to [query db sql]. This prepared statement can then be called with the
      actual arguments (if any) and the resultset row decoder:

      {[let add_person =
          query db (sql "insert into people (name, age) values (%a, %a)" placeholder 0 placeholder 1)
        let add_person name age = add_person Arg.[text name; int age] unit]}

      @raise Invalid_argument if trying to create multiple prepared statements
        for the same SQL query in PostgreSQL. To avoid this, just create the
        prepared statement {e once only} and call it whenever needed, as shown
        above. *)

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

  val unit : (row, unit) ret
  (** [unit] indicates that the query doesn't return any meaningful output. *)

  val ret : (row -> 'a) -> (row, 'a Seq.t) ret
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

  val sql : ('a, Format.formatter, unit, string) format4 -> 'a
  (** Helper to construct SQL query strings using [placeholder]s. *)

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

module Make (Sql : Sql) : S with type db = Sql.db and type arg = Sql.arg =
struct
  type db = Sql.db
  type arg = Sql.arg

  let sql = Format.asprintf

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
    query db "begin" [] unit;
    match f () with
    | r ->
      query db "commit" [] unit;
      r
    | exception e ->
      query db "rollback" [] unit;
      raise e

  let slurp file =
    let inc = open_in file in
    Fun.protect
      ~finally:(fun () -> close_in inc)
      (fun () -> really_input_string inc (in_channel_length inc))

  exception Bad_migration of string

  let migrate db =
    exec_script db
      "create table if not exists migration (
         filename varchar(1024) not null primary key,
         script text not null,
        applied_at timestamp
       )";
    let mark_ok =
      query db
        (sql
           "insert into migration (filename, script, applied_at)
            values (%a, %a, current_timestamp)"
           placeholder 0 placeholder 1)
    in
    let migrated =
      query db (sql "select 1 from migration where filename = %a" placeholder 0)
    in
    let migrated filename =
      0
      |> bool
      |> ret
      |> migrated [filename]
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
           | () -> mark_ok Arg.[arg_filename; text script] unit
           | exception Failure msg -> raise (Bad_migration msg)
end
