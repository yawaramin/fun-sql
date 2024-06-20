open! Fun_sqlite
type t = {
  name: string ;
  age: int option ;
  height_m: float }[@@deriving funsql]
include
  struct
    let _ = fun (_ : t) -> ()
    let ret =
      Fun_sql.ret
        (fun row ->
           {
             name = (text 0 row);
             age = (opt int 1 row);
             height_m = (float 2 row)
           })
    let _ = ret
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
