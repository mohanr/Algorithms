
type 'a s_tree = Leaf | Node of 'a node
and 'a node = { value : 'a; left : 'a s_tree; right : 'a s_tree; }

let node_value n = 
  match n with
  |Leaf ->  0
  | Node {value; _}-> value

let insert_with_value=
  Node {
    value = 2;
    left = Node {value = 1; left = Leaf; right = Leaf};
    right = Node {value = 3; left = Leaf; right = Leaf}
  }

let rec print_sTree (sTree : int s_tree ) (d : int) : unit =
  match sTree with
  |Leaf -> () 
  | Node { left  ;value ;  right} ->
    print_sTree right (d + 1);
    for __i=0 to  (d - 1) do
      Printf.printf "  "
    done;
    Printf.printf "%d\n" value;
    print_sTree left  (d+1) 

type 'a splay_tree = Leaf | Node of 'a node1
and 'a node1 = { key : 'a;value : 'a; mutable left : 'a splay_tree option; mutable right : 'a splay_tree option; }
type 'a t = 'a splay_tree option ref

let rec print_splaytree (t : int splay_tree option ref) (d : int) : unit =
  match !t with
  | Some Leaf -> () 
  | None -> () 
  | Some Node {left; key; value=_; right}->
    print_splaytree  (ref right) (d + 1);
    for __i=0 to  (d - 1) do
      Printf.printf "  "
    done;
    Printf.printf "%d\n" key;
    print_splaytree (ref left)  (d+1) 

let insert_with_key_value: int splay_tree option ref=
  ref (Some ( Node {
      key = 1;
      value = 2;
      left = Some (Node {key=3; value=1; left=Some (Leaf); right=Some (Leaf)});
      right = Some (Node {key=4; value=3; left=Some (Leaf); right=Some (Node {
          key = 2;
          value = 2;
          left = Some (Node {key=3; value=1; left=Some (Leaf); right=Some (Leaf)});
          right = Some (Node {key=4; value=3; left=Some (Leaf); right=Some (Leaf)})
        })})
    }))




let rec insert_key (k : int ) (t : int splay_tree option ref) : int splay_tree option ref=
 match !t with
  | None |Some Leaf ->
    let () = Printf.printf "(insert_key)Inserting new node %d\n" k  in

    let new_node = Node { key = k; value = 0; left = None; right = None } in
    t := Some new_node;
    t
    
  | Some tree  ->

      let  insert_node tree =

      match tree with
      |  Node old_key ->
        begin match old_key with
          |  ok  ->
            if k > ok.key then(
              match ok.right with
              | None | Some Leaf ->
                let () = Printf.printf "(insert_key)Inserting %d\n" k  in

              let r = ref (Some (Node { key = k ;value = 0 ; right = Some Leaf; left = Some Leaf} ))in
               ok.right <- !r;
               t
             | Some _r ->
               let () = Printf.printf "(insert_key)Inserting %d\n" k  in

             insert_key k (ref (ok.right ))
             )
            else 
            if k < ok.key then(
              match ok.left with
              | None | Some Leaf ->
                let () = Printf.printf "(insert_key)Inserting %d\n" k  in

               let l = ref (Some (Node { key = k ;value = 0 ; right = Some Leaf; left = Some Leaf} ))in 
              ok.left <- !l;
              t 
             | Some _l -> 
               let () = Printf.printf "(insert_key)Inserting %d\n" k  in

             insert_key k (ref (ok.left)); 
            )
          else
             t
        end;

     |Leaf -> t
      in

      insert_node  tree

