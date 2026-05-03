

let%expect_test "Test insert"=

  let x = 3 in
  Printf.printf "%d\n" x;
  [%expect {| 3 |}]
