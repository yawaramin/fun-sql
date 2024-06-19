open Fun_sqlite

type t =
  { name : string;
    age : int option
  }
[@@deriving funsql]
