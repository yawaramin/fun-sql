let test_arg msg arg =
  Printf.printf "test_arg: %s: %s
" msg (Sqlite3.Data.to_string_debug arg)

open Fun_sqlite

let row =
  Arg.[|blob "hello"; bool true; float 1.; int 1; int64 1L; text "hello"|]

let () =
  test_arg "blob" (Arg.blob "world");
  test_arg "bool" (Arg.bool false);
  test_arg "float" (Arg.float 1.);
  test_arg "int" (Arg.int 1);
  test_arg "int32" (Arg.int32 1l);
  test_arg "int64" (Arg.int64 1L);
  test_arg "nativeint" (Arg.nativeint 1n);
  test_arg "opt int" Arg.(opt int None);
  test_arg "text" (Arg.text "hello");

  Printf.printf "test_row: %s %b %f %d %Ld %s
" (text 0 row) (bool 1 row)
    (float 2 row) (int 3 row) (int64 4 row) (text 5 row)
