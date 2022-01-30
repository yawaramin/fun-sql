let ( let* ) result f = match result with
  | Ok x -> f x
  | (Error _) as error -> error

open Sqlite3

type ('a, 's) t = stmt -> 's -> 'a
type 'a param = (('a, int) t -> 'a, int) t
type 'a mapper = Data.t array -> ('a, string) result

let result_of = function
  | Rc.OK
  | DONE -> Ok ()
  | rc -> Error (Rc.to_string rc)

let query db sql =
  let stmt = prepare db sql in
  fun k -> k stmt 1

let int i stmt pos =
  ignore (bind_int stmt pos i);
  fun k -> k stmt (succ pos)

let text t stmt pos =
  ignore (bind_text stmt pos t);
  fun k -> k stmt (succ pos)

let ret mapper stmt _ =
  let rows () = match step stmt with
    | ROW ->
      begin match stmt |> row_data |> mapper with
      | Ok obj -> Some (obj, ())
      | Error msg -> invalid_arg msg
      end
    | DONE ->
      None
    | rc ->
      let rc_str = Rc.to_string rc in
      invalid_arg ("Sql.rows: invalid operation: " ^ rc_str)
  in
  let seq = Seq.unfold rows () in
  ignore (reset stmt);
  seq

let ret_unit stmt _ =
  let* () = result_of @@ step stmt in
  result_of @@ reset stmt

let ret_int64 stmt a =
  let mapper = function
    | [|Data.INT i|] -> Ok i
    | _ -> Error "Expected int"
  in
  ret mapper stmt a
