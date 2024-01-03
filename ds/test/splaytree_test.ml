
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
  let kv = Bloomfilter__.Splaytree.insert_with_key_value in
    Bloomfilter__.Splaytree.print_splaytree  kv 1 ;
    [%expect {|
            4
          2
            3
        4
      1
        3 |}]



let%expect_test _=
  let tree =  ref None in
  let _ = Bloomfilter__.Splaytree.insert_key 1 tree in
  let _ = Bloomfilter__.Splaytree.insert_key 4 tree in
  let _ = Bloomfilter__.Splaytree.insert_key 2 tree in
  let _ = Bloomfilter__.Splaytree.insert_key 3 tree in

  Bloomfilter__Splaytree.print_splaytree  tree 1; 
  [%expect {|
    (insert_key)Inserting new node 1
    (insert_key)Inserting 4
    (insert_key)Inserting 2
    (insert_key)Inserting 2
    (insert_key)Inserting 3
    (insert_key)Inserting 3
    (insert_key)Inserting 3
        4
            3
          2
      1
       |}]


let%expect_test _=
  let tree =  ref None in
  let _ = Bloomfilter__.Splaytree.insert_key 5 tree in
  let _ = Bloomfilter__.Splaytree.insert_key 9 tree in
  let _ = Bloomfilter__.Splaytree.insert_key 13 tree in
  let _ = Bloomfilter__.Splaytree.insert_key 11 tree in
  let _ = Bloomfilter__.Splaytree.insert_key 1 tree in

  Bloomfilter__Splaytree.print_splaytree  tree 1; 
  [%expect {|
    (insert_key)Inserting new node 5
    (insert_key)Inserting 9
    (insert_key)Inserting 13
    (insert_key)Inserting 13
    (insert_key)Inserting 11
    (insert_key)Inserting 11
    (insert_key)Inserting 11
    (insert_key)Inserting 1
          13
            11
        9
      5
        1
       |}]


let tree_from_node (node:int Bloomfilter__.Splaytree.node1 option): int Bloomfilter__.Splaytree.splay_tree option ref=
  match node with
  | None -> 
    (ref (Some (Bloomfilter__.Splaytree.Node{ key = 0;value=0; left = None; right = None })))
| Some n ->
    match n with
      | { key ; value;left; right } -> 
        let newNode = (ref (Some (Bloomfilter__.Splaytree.Node {key;value;left;right}))) in
        newNode

