open Sqlite3

type ('a, 's) t
type 'a param = (('a, int) t -> 'a, int) t
type 'a mapper = Data.t array -> ('a, string) result

val query : db -> string -> ('a, int) t -> 'a
val int : int -> 'a param
val text : string -> 'a param

val ret : 'a mapper -> ('a Seq.t, _) t
val ret_unit : ((unit, string) result, _) t
val ret_int64 : (int64 Seq.t, _) t
