
let%expect_test _=
  let x = 3 in
  Printf.printf "%d\n" x;
  [%expect {| 3 |}]


type 'a r_tree = Leaf | Node of 'a node1
and 'a node1 = { value : 'a; left : 'a r_tree; right : 'a r_tree; }

let rec check_splay_tree t = 
  match t with
  |Leaf ->  false
  | Node {left; value = v; right}->
    match left, right with
    | Node { left = _; value = v0;  _}, Node {left =  _; value = v1;  _} -> v == v1 + v0 + 1 
    | Node { left ;   _}, Leaf -> check_splay_tree left
    | Leaf, Node { left = _ ;value = _;  right} -> check_splay_tree right
    | _ -> false



let insert=
  Node {
    value = 2;
    left = Node {value = 1; left = Leaf; right = Leaf};
    right = Node {value = 3; left = Leaf; right = Leaf}
  }


let%expect_test _=
  Printf.printf  "%s" (string_of_bool (check_splay_tree insert));
  [%expect {| false |}]

let%expect_test _=
   Bloomfilter__Splaytree.print_sTree Bloomfilter__.Splaytree.insert_with_value 0;
  [%expect {|
      3
    2
      1 |}]

let%expect_test _=
  let tree =  ref None in
  let i = Bloomfilter__.Splaytree.insert_key 1 tree in
  let j = Bloomfilter__.Splaytree.insert_key 2 i in
  let _k = Bloomfilter__.Splaytree.insert_key 3 j in
  Bloomfilter__Splaytree.print_splaytree  tree 1; 
  [%expect {|
        3
      2
    1 |}]

let%expect_test _=
  let tree =  ref None in
  let i = Bloomfilter__.Splaytree.insert_key 6 tree in
  let j = Bloomfilter__.Splaytree.insert_key 9 i in
  let k = Bloomfilter__.Splaytree.insert_key 2 j in
  let l = Bloomfilter__.Splaytree.insert_key 3 k in
  let m = Bloomfilter__.Splaytree.insert_key 6 l in
  let _n = Bloomfilter__.Splaytree.insert_key 16 m in
  Bloomfilter__Splaytree.print_splaytree  tree 1; 
  [%expect {|
        9
      6
              16
            6
          3
        2 |}]


let%expect_test _=
  let tree =  ref None in
  let i = Bloomfilter__.Splaytree.insert_key 1 tree in
  let j = Bloomfilter__.Splaytree.insert_key 2 i in
  let _k = Bloomfilter__.Splaytree.insert_key 3 j in
  let _t = Bloomfilter__.Splaytree.splay 1 tree in
  (* Bloomfilter__Splaytree.print_splaytree  tree 1;  *)
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Not_found
  Raised at Bloomfilter__Splaytree.splay.loop in file "lib/stree/splaytree.ml", line 141, characters 60-75
  Called from Bloomfilter__Splaytree.splay in file "lib/stree/splaytree.ml", line 175, characters 10-38
  Called from Bloomfilter_test__Splaytree_test.(fun) in file "test/splaytree_test.ml", line 76, characters 11-47
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Looping Getting key Key 1 Exiting
        3
      2
    1 |}]
