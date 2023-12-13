
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

let get_nodekey node= 
  match node with
  |None -> 0
  |Some node ->
    match node with
    | Leaf -> 0
    | Node n ->  
      match n with
      | { key ; value = _;left  = _; right = _ } -> key


let left_point node= 
match node with
  |None -> None
  |Some node ->
    match node with
    | Leaf -> None
    | Node n -> Some n 


let splayed_tree trnode l r n=
  Printf.printf "Exiting\n";
  !l.right <- n.right;
  !r.left <-  n.left;
  match trnode with
      | { key ; value;left; right } as t -> 
         t.left <-  n.right;
         t.right <- n.left;
         print_splaytree (ref (Some (Node {key;value;left;right}))) 1

let splay (i : int ) (t : int splay_tree option ref) =
  try
  let n = { key = 0;value=0; left = None; right = None } in
  let l = ref n in
  let r = ref n in
    let get_key node = 
      Printf.printf "Getting key ";
      match node with
      | Some y_node -> 
        begin match y_node with
          | { key ; value = _;left  = _; right = _ } -> key
        end;
      | None -> 0 in
  let () = Printf.printf "Looping " in
  let rec loop tr =
    match !tr with
    | None  -> ()
    | Some trnode ->
      let y = ref None in
      match !trnode with 
      | { key = k; value=0;left  = left_node; right = right_node } ->
        let key = get_key (Some !trnode)  in
        let () = Printf.printf "Key %d " key in
          if i < key then (
            let lkey = get_nodekey left_node  in
            if i < lkey then (
              y :=  left_node;
              begin match !y with
                | Some y_node ->
                  begin match y_node with
                    |Node r -> 
                      ref left_node := r.right;
                      begin match r.right with
                            | None ->() 
                            |Some cn ->
                              match cn with
                              | Leaf -> ()
                              | Node n ->  
                                ref n := !trnode;
                                trnode := r;
                                loop (ref (Some trnode))
                      end;
                    | Leaf -> ()
                  end;
          | None -> splayed_tree !trnode l r n ;  raise Not_found
              end;
            );
                    t := !trnode.left;
                    r := !trnode ;
            let rn = !r in
            let lp = left_point rn.left in
            ref lp := Some !trnode;
          (* ) else if i = k then trnode := !trnode *)

          ) else if i = k then( splayed_tree !trnode l r n ;raise Not_found;)
          else if i > key then (
            let rkey = get_nodekey right_node  in
            if i > rkey then (
              y :=  right_node;
              begin match !y with
                | Some y_node ->
                  begin match y_node with
                    |Node l -> 
                      ref right_node := l.left;
                      begin match l.left with
                        | None ->() 
                        |Some cn ->
                              match cn with
                              | Leaf -> ()
                              | Node n ->  
                                ref n := !trnode;
                                trnode := l;
                                loop (ref (Some trnode))
                      end;
                    | Leaf -> ()
                  end;
               | None -> splayed_tree !trnode l r n ;  raise Not_found
              end;
            );
              )
        | _ -> ()
  in
  match !t with
  | None -> None
  | Some node ->
    match node with
        | Leaf -> None 
        | Node root ->
          loop (ref (Some (ref root)));
              Some root

with Exit -> let () = Printf.printf "Exiting\n" in None

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


