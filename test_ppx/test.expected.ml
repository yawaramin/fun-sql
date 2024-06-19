open Fun_sqlite
type t = {
  name: string ;
  age: int option }[@@deriving funsql]
include
  struct
    let _ = fun (_ : t) -> ()
    let ret = ret (fun row -> { name = (text 0 row); age = (opt int 1 row) })
    let _ = ret
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
