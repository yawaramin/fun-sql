open! Fun_sqlite

type t =
  { name : string;
    age : int option;
    height_m : float
  }
[@@deriving funsql]
