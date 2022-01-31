let ( let* ) result f = match result with
  | Ok x -> f x
  | (Error _) as error -> error

open Sqlite3

type ('a, 's) t = stmt -> 's -> 'a
type 'a param = (('a, int) t -> 'a, int) t
type 'a decoder = Data.t array -> ('a, string) result
type 'a encoder = 'a -> Data.t list

let result_of = function
  | Rc.OK
  | DONE -> Ok ()
  | rc -> Error (Rc.to_string rc)

let query db sql =
  let stmt = prepare db sql in
  fun k -> k stmt 1

let next pos stmt k = k stmt (succ pos)

let text value stmt pos =
  ignore (bind_text stmt pos value);
  next pos stmt

let bool value stmt pos =
  ignore (bind_bool stmt pos value);
  next pos stmt

let int value stmt pos =
  ignore (bind_int stmt pos value);
  next pos stmt

let nativeint value stmt pos =
  ignore (bind_nativeint stmt pos value);
  next pos stmt

let int32 value stmt pos =
  ignore (bind_int32 stmt pos value);
  next pos stmt

let int64 value stmt pos =
  ignore (bind_int64 stmt pos value);
  next pos stmt

let double value stmt pos =
  ignore (bind_double stmt pos value);
  next pos stmt

let blob value stmt pos =
  ignore (bind_blob stmt pos value);
  next pos stmt

let ret decoder stmt _ =
  let rows () = match step stmt with
    | ROW ->
      begin match stmt |> row_data |> decoder with
      | Ok obj -> Some (obj, ())
      | Error msg -> invalid_arg msg
      end
    | DONE ->
      None
    | rc ->
      let rc_str = Rc.to_string rc in
      failwith ("Sql.rows: invalid operation: " ^ rc_str)
  in
  let seq = Seq.unfold rows () in
  ignore (reset stmt);
  seq

let ret_unit stmt _ =
  let* () = result_of @@ step stmt in
  result_of @@ reset stmt

let ret_int64 stmt a =
  let mapper = function
    | [|Data.INT value|] -> Ok value
    | _ -> Error "Expected int"
  in
  ret mapper stmt a

let ret_float stmt a =
  let mapper = function
    | [|Data.FLOAT value|] -> Ok value
    | _ -> Error "Expected float"
  in
  ret mapper stmt a

let ret_text stmt a =
  let mapper = function
    | [|Data.TEXT value|] -> Ok value
    | _ -> Error "Expected text"
  in
  ret mapper stmt a

let ret_blob stmt a =
  let mapper = function
    | [|Data.BLOB value|] -> Ok value
    | _ -> Error "Expected blob"
  in
  ret mapper stmt a

(* We have to parse the value tuple of the insert statement to be able to
   multiply it a number of times if needed for a batch insert. *)

let sp_r = Str.regexp_string " "
let wsp_r = Str.regexp
  @@ Printf.sprintf "[%c%c%c]" (Char.chr 9) (Char.chr 10) (Char.chr 13)
let values_r = Str.regexp_case_fold
  {|^\(insert into .* values *\)\(([^)]+)\)\(.*\)$|}

let pre_pos = 1
let tuple_pos = 2
let post_pos = 3

let insert db sql encoder objs =
  let sql = sql
    |> String.trim
    |> Str.global_replace wsp_r " "
    |> Str.global_replace sp_r " "
  in
  if Str.string_match values_r sql 0 then
    let tuple = Str.matched_group tuple_pos sql in
    let tuples = objs |> List.map (fun _ -> tuple) |> String.concat "," in
    let sql =
      Str.matched_group pre_pos sql ^ tuples ^ Str.matched_group post_pos sql
    in
    let stmt = prepare db sql in
    let data = objs |> List.map encoder |> List.concat in
    ignore (bind_values stmt data);
    fun k -> k stmt 0
  else
    failwith ("insert: expected valid statement, got '" ^ sql ^ "'")
