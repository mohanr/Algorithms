
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

let splay (i : int ) (t : int splay_tree option ref) =
  let n = { key = 0;value=0; left = None; right = None } in
  let l = ref n in
  let r = ref n in
  let get_key node = match node with
    | Some n -> n.key
    | None -> 0 in
  let  loop t =
    match !t with
    | None -> ()
    | Some node ->
      let y = ref None in
      (* while true do *)
       match !node with 
        | { key = k; value=0;left = Some left_node; right = _ } as n when i < k ->
          let left_key = get_key (Some !node )in
          if i < left_key then (
            y := Some left_node;
            !node.left <- !y;
            begin match !y with
            | Some y_node ->
              begin match y_node with
              |Node r -> r.right <- !node.left;
              | Leaf -> ()
              end;
            | None -> ()
            end;
            node := !r
          ) else if i = k then node := !node
              else (
                begin match n.right with
                  | None -> ()
                  |  Some right ->
                    match right with
                    |Node r -> 
                      let right_key = get_key (Some r) in
                      if i > right_key then (
                        y := Some right;
                        !node.right <- !y;
                        begin match !y with
                        | None -> ()
                        | Some y_node ->
                              begin match y_node with
                                |Node _l -> r.left <- !node.left;
                                | Leaf -> ()
                              end;
                        end;
                        node := !l
                      )
                    | _-> ()
                end;
              )
        | _ -> ()
  (*     done *)
  in
  l := { key = 0; value = 0; left = None; right =  !t };
  r := !l;
  match !t with
  | None -> None
  | Some node ->
    match node with
        | Leaf -> None 
        | Node root ->
          loop (ref (Some (ref root)));
          match !t with
          | None -> None
          | Some node ->
            !l.right <- n.right;
            !r.left <-  n.left;
            match node with
            | Leaf -> None
            | Node localnode ->
              localnode.left <-  n.right;
              localnode.right <- n.left;
              Some node



let rec insert_key (k : int ) (t : int splay_tree option ref) : int splay_tree option ref=
 match !t with
  | None |Some Leaf ->
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
              let r = ref (Some (Node { key = k ;value = 0 ; right = Some Leaf; left = Some Leaf} ))in
               ok.right <- !r;
               t
             | Some _r ->
             insert_key k (ref (ok.right ))
             )
            else 
            if k < ok.key then(
              match ok.left with
              | None ->
               let l = ref (Some (Node { key = k ;value = 0 ; right = Some Leaf; left = Some Leaf} ))in 
              ok.left <- !l;
              t 
             | Some _l ->
             insert_key k (ref (ok.left)); 
            )
          else
             t
        end;
     |Leaf ->t
    in
    insert_node tree


