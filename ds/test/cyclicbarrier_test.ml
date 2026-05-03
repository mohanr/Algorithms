open Bloomfilter__Cyclicbarrier


let%expect_test _=
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun _sw ->
  let b = create_cyclic_barrier 10 in
  Printf.printf "%d\n" b.participants ;
  Printf.printf "%d\n" (wait_list b) ;
  Printf.printf "%s\n" (Bool.to_string (is_broken b)) ;

  [%expect {|
    10
    0
    false
    |}]

let%expect_test "test concurrency"=
  spawn_fibers;
  [%expect {| |}]
