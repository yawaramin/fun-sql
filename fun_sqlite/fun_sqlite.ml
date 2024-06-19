(* Copyright 2024 Yawar Amin

   This file is part of fun-sql.

   fun-sql is free software: you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   fun-sql is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   fun-sql. If not, see <https://www.gnu.org/licenses/>. *)

include Impl
include Fun_sql.Make (Impl)

(* We have to parse the value tuple of the insert statement to be able to
   multiply it a number of times if needed for a batch insert. *)

let sp_r = Str.regexp " +"

let wsp_r =
  Str.regexp
  @@ Printf.sprintf "[%c%c%c]+" (Char.chr 9) (Char.chr 10) (Char.chr 13)

let values_r =
  Str.regexp_case_fold {|^\(insert into .* values *\)\(([^)]+)\)\(.*\)$|}

let pre_pos = 1
let tuple_pos = 2
let post_pos = 3

let batch_insert db sql objs obj_args =
  let sql =
    sql
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
    query db sql (objs |> List.map obj_args |> List.flatten)
  else
    failwith sql
