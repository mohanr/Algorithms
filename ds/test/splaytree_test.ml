
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
   Bloomfilter__Splaytree.print_sTree Bloomfilter__.Splaytree.insert 0;
  [%expect {|
      3
    2
      1 |}]



(*     int splay_main() { *)
(*     /* A sample use of these functions.  Start with the empty sTree,         */ *)
(*                                                     /* insert some stuff into it, and then delete it                        */ *)
(*                                                                                            sTree * root; *)
(* int i; *)
(* root = NULL;              /* the empty sTree */ *)
(*                           for (i = 0; i < 1024; i++) { *)
(*                               root = insert((541*i) & (1023), root); *)
(*                               check_sTree(root); *)
(*                             } *)
(*                               for (i = 0; i < 1024; i++) { *)
(*                                   root = splay_delete((541*i) & (1023), root); *)
(*                                   check_sTree(root); *)
(*                                 } *)

(*                                   printf("root=%p\n", root); *)
(*                                 root = insert(1, root); *)
(*                                 check_sTree(root); *)
(*                                 root = insert(3, root); *)
(*                                 root = insert(5, root); *)
(*                                 root = insert(12, root); *)
(*                                 root = insert(8, root); *)
(*                                 root = insert(6, root); *)
(*                                 print_sTree(root, 3); *)
(*                                 check_sTree(root); *)
(*                                 root = splay_delete(2, root); *)
(*                                 printf("\n"); *)
(*                                 print_sTree(root, 3); *)
(*                                 free_sTree(root); *)

(*                                 return (0); *)
(* } *)
