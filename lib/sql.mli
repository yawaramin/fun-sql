open Sqlite3

type ('a, 's) t
(** A query component that can be composed with query parameters and return
    types. *)

type 'a param = (('a, int) t -> 'a, int) t
(** A query parameter. The function with [int]s represents that the parameter is
    positionally bound, and the position index incremented when that happens.
    The user doesn't need to manually track the numbers and positions of
    bindings, they just need to provide the corresponding binding values for any
    positional variables in the SQL query, represented by [?]. *)

type 'a decoder = Data.t array -> ('a, string) result
(** A decoder of resultset rows from the database. A row is represented as an
    array of data. This is decoded into the desired type. *)

type 'a encoder = 'a -> Data.t list
(** An encoder from a specific type to a row to be inserted into the database. *)

val query : db -> string -> ('a, int) t -> 'a
(** [query db sql ...] executes a query [sql] against the database [db]. The
    arguments to be bound to the query, if any, are passed in following [sql].
    The return type of the query is then passed in as the final argument. *)

val insert : db -> string -> 'a encoder -> 'a list -> ('b, int) t -> 'b
(** [insert db sql encoder objs ret_typ] inserts into the database [db], running
    the query [sql], the row tuples obtained by running the [encoder] on the
    given data [objs]. The return type of the query is passed in as [ret_type].
    If the query doesn't return anything, i.e. it doesn't have a [returning]
    clause, [ret_unit] should be passed in. *)

(** {2 Basic encoders} *)

(*
val text : text encoder
val bool : bool encoder
val int : int encoder
val nativeint : nativeint encoder
val int32 : int32 encoder
val int64 : int64 encoder
val double : float encoder
val blob : string encoder
*)

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

val ret : 'a decoder -> ('a Seq.t, _) t
(** [ret decoder] is a custom decoder for a resultset into a sequence of values
    of the type decoded by [decoder].

    @raise Invalid_argument if any row cannot be decoded.
    @raise Failure if an unexpected result code is encountered. *)

val ret_unit : ((unit, string) result, _) t
val ret_int64 : (int64 Seq.t, _) t
val ret_float : (float Seq.t, _) t
val ret_text : (string Seq.t, _) t
val ret_blob : (string Seq.t, _) t
