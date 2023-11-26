
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
  | Some Node {left; key=_; value; right}->
    print_splaytree  (ref right) (d + 1);
    for __i=0 to  (d - 1) do
      Printf.printf "  "
    done;
    Printf.printf "%d\n" value;
    print_splaytree (ref left)  (d+1) 

let insert_with_key_value=
  ref( Node {
      key = 2;
      value = 2;
      left = Some (Node {key=3; value=1; left=Some (Leaf); right=Some (Leaf)});
      right = Some (Node {key=4; value=3; left=Some (Leaf); right=Some (Leaf)})
    
    })
let splay i t =
  match !t with
  |Leaf ->  None
  | Node { key=_; left  ;value=_ ;  right} -> 
    match left, right with
    | Some Node _,Some Node {left=_; key=_; value=_; right=_}->None
    | Some Node {left; key; value=_; right},Some Leaf | Some Node {left; key; value=_; right}, _ -> 
      if i < key then (
        let y = left in
        let _l = right in
        let _right = t in
        let left = y in 
        left (* Return 'left' value *)
      ) else (
        let left = t in
        let _right = t in 
        let newT = left in
        Some !newT (* Returning 'left' value wrapped in 'Some' *)
      )
      | Some Leaf,Some Node _-> None 
      | Some Leaf,Some Leaf->  None
      | _ ,Some Leaf -> None
      | Some Leaf,None -> None
      |(None, Some (Node _))-> None
      |None, None ->None 

