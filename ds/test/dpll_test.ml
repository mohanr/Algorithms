open Bloomfilter__.Dpll

let%expect_test _=

  let buf = Buffer.create 5 in
  (List.iter (Buffer.add_int8 buf) (clauses "-5 -4 -1 -2" ));
  Printf.printf "%s" (Buffer.contents buf);
  [%expect {|
    -5 -4 -1 -2 |}]
