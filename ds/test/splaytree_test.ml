
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
  let _i = Bloomfilter__.Splaytree.insert_key 1 tree in
  let _j = Bloomfilter__.Splaytree.insert_key 2 tree in
  let _k = Bloomfilter__.Splaytree.insert_key 3 tree in
  Bloomfilter__Splaytree.print_splaytree  tree 1; 
  [%expect {|
    Looping Getting key Key 1 Key 1 is greater than 2 Printing Splayed Tree
      1
    Looping Getting key Key 1 Key 1 is greater than 3 Printing Splayed Tree
      1
      1 |}]


let tree_from_node (node:int Bloomfilter__.Splaytree.node1 option): int Bloomfilter__.Splaytree.splay_tree option ref=
  match node with
  | None -> 
    (ref (Some (Bloomfilter__.Splaytree.Node{ key = 0;value=0; left = None; right = None })))
| Some n ->
    match n with
      | { key ; value;left; right } -> 
        let newNode = (ref (Some (Bloomfilter__.Splaytree.Node {key;value;left;right}))) in
        newNode

let%expect_test _=
  let tree =  ref None in
  let _i = Bloomfilter__.Splaytree.insert_key 1 tree in
  let j = Bloomfilter__.Splaytree.splay 2 tree in
  let _k = Bloomfilter__.Splaytree.insert_key 2 (tree_from_node j) in
  let l = Bloomfilter__.Splaytree.splay 3 tree in
  let _m = Bloomfilter__.Splaytree.insert_key 3 (tree_from_node l)  in
   Bloomfilter__Splaytree.print_splaytree  tree 1;  
  [%expect{|
    Looping Getting key Key 1 Key 1 is greater than 2 Printing Splayed Tree
      1
    Looping Getting key Key 1 Key 1 is greater than 2 Printing Splayed Tree
      1
    Looping Getting key Key 1 Key 1 is greater than 3 Printing Splayed Tree
      1
    Looping Getting key Key 1 Key 1 is greater than 3 Printing Splayed Tree
      1
      1 |}]
