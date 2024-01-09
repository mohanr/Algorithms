module type ORDERED_FUNCTIONAL_SET = sig
  (* Overview: a "set" is a set of distinct
   * elements of type "elem". Each element is     
   * identified by a unique key, which may be the 
   * same as the element itself. Two elements are 
   * considered distinct if they have different   
   * keys.  Keys are a totally ordered set.       
   *
   * A set can be used to represent an ordinary   
   * set if key = elem.  It can be used to        
   * represent a mapping if elem = key * value.   
   *
   * For example, if key and elem are int,        
   * then a set might be {1,-11,0}, {}, or        
   * {1001}. If key is string and elem is int,    
   * a set could be {("elephant", 2), ("rhino",   
   * 25), ("zebra", 2)} *)
  (* https://github.com/shwestrick/pure-splay/blob/master/BottomUpSplay.sml *)

  type key
  type elem
  type set
  type order = LESS | EQUAL | GREATER
  (* compare(k1,k2) reports the ordering of k1 and k2. *)
  val compare : key -> key -> order
  (* keyOf(e) is the key of e. *)
  val keyOf: elem -> key
  val intOf: elem -> int
  (* empty() is the empty set. *)
  val empty : unit -> set
  (* Effects: add(s,e) is s union {e}. Returns true
   * if e already in s, false otherwise. *)
  val add: set -> elem -> set * bool
  (* val add: set -> elem -> (int * tree ref) * bool *)

  (* type tree = Empty | Node of tree * elem * tree *)
  (* type node = tree * elem * tree *)
  (* val add: int * tree ref -> elem -> (int * (tree * elem * tree) ref) * bool *)

  (* remove(s,k) is (s',eo) where s' = s - {k} (set difference)
   * and eo is either SOME e if there is an e in s
   * where k is e's key, or NONE otherwise. *)
  (* val remove: set * key -> set * elem option *)

  (* lookup(s,k) is SOME e where k = keyOf(e), or NONE if
   * the set contains no such e. *)
  (* val lookup: set * key -> elem option *)

  (* size(s) is the number of elements in s. *)
  (* val size: set -> int *)

  val size : 'a -> 'b -> 'a
  (* Ordered set operations *)

  (* first(s) is SOME of the element of s with the smallest key,
   * or NONE if s is empty. *)
  (* val first: set -> elem option *)

  (* last(s) is SOME of the element of s with the largest key,
   * or NONE if s is empty. *)
  (* val last: set -> elem option *)

  (* A fold operation on ordered sets takes a key argument
   * that defines the element where the fold starts. *)
  type 'b folder = ((elem*'b)->'b) -> 'b -> key -> set -> 'b
  (* fold over the elements in key order. *)
  val fold_forward: 'b folder
  (* fold over the elements in reverse key order. *)
  val fold_backward: 'b folder
(*   val print: set -> unit *)
end

module type ORDERED_SET_PARAMS = sig
  type key
  type elem
  val keyOf: elem -> key
  val intOf: elem -> int
  type order = LESS | EQUAL | GREATER
  (* val compare: key -> key -> order *)

  val toString: elem -> string
end

module  SplayTree (Params : ORDERED_SET_PARAMS)
  : ORDERED_FUNCTIONAL_SET with type key = Params.key and
                                  type elem = Params.elem = struct
  type key = Params.key
  type order = LESS | EQUAL | GREATER
  type elem = Params.elem
  (* let compare = Params.compare *)

  let keyOf = Params.keyOf
  let intOf = Params.intOf
  type tree = Empty | Node of tree * elem * tree
  type node = tree * elem * tree
  (* Representation invariant: given a node (L, V, R),
   * All values in the left subtree L are less than V, and
   * all values in the right subtree R are greater than V, and
   * both L and R also satisfy the RI.
   *)
  type set = int * (tree ref)
  (* Representation invariant: size is the number of elements in
   * the referenced tree. *)
  let compare (k1: key) (k2: key) : order =
    if k1 < k2 then LESS
    else if k1 = k2 then EQUAL
    else GREATER
  let empty() = (0, ref Empty)

  (* splay(n,k) is a BST node n' where n' contains
   * all the elements that n does, and if an
   * element keyed by k is in under n, #value n'
   * is that element.  Requires: n satisfies the
   * BST invariant.
   *)
  let rec splay (l, v, r) (k:Params.key) =
    match compare k (keyOf (v)) with
    | EQUAL -> (l, v, r)
        | LESS ->
        (match l with
             | Empty -> (l, v, r) (* not found *)
             | Node (ll, lv, lr) ->
               match compare k (keyOf (lv)) with
                 | EQUAL -> (ll, lv, Node(lr, v, r)) (* 1: zig *)
                 | LESS ->
                 (match ll with
                      | Empty -> (Empty, lv, Node(lr, v, r))
				(* not found *)
                      | Node (lln, lvn, lrn) as n -> (* 2: zig-zig *)
                        let (lll, llv, llr) = splay (lln, lvn, lrn) k  in
                        (lll,llv,Node(llr,lv,Node(lr,v,r)))
                      )
                | GREATER ->
                    (match lr with
                        | Empty -> (ll, lv, Node(Empty, v, r))
                        |Node (lln, lvn, lrn) as n ->  (* 3: zig-zag *)
                          let (lrl, lrv, lrr) = splay (lln, lvn, lrn) k  in
                           (Node(ll,lv,lrl),lrv,Node(lrr,v,r))
                         ))
        | GREATER ->
	(match r with
	    | Empty -> (l, v, r) (* not found *)
	    | Node (rl, rv, rr) ->
	     match compare k (keyOf (rv)) with
		     |EQUAL -> (Node(l,v,rl),rv,rr) (* 1: zag *)
         | GREATER ->
		(match rr with
		   | Empty -> (Node(l,v,rl),rv,rr) (* not found *)
		 | Node (lln, lvn, lrn) as n -> (* 3: zag-zag *)
		 let (rrl, rrv, rrr) = splay (lln, lvn, lrn) k  in
			(Node(Node(l,v,rl),rv,rrl),rrv,rrr)
			)
		| LESS ->
		(match rl with
	| Empty -> (Node(l,v,rl),rv,rr) (* not found *)
 | Node (lln, lvn, lrn) as n -> (* 2: zag-zig *)
	 let (rll, rlv, rlr) = splay (lln, lvn, lrn) k  in
			(Node(l,v,rll),rlv,Node(rlr,rv,rr))
			))

  let lookup (size,tr) k =
    match !tr with
        |Empty -> None
        | Node (lln, lvn, lrn) as n ->
          let   (r1,v,r2) as n' = splay (lln, lvn, lrn) k in
          tr := Node (r1,v,r2);
          if compare k  (keyOf(v)) = EQUAL then Some v
          else None

(*   let rec add_tree  (t: tree) (e: elem) :node * bool = *)
(*     match t with *)
(*     |Empty -> ((Empty, e, Empty), false) *)
(*        | Node (l,v,r) -> *)
(*          (match compare (keyOf(v)) (keyOf(e)) with *)
(*         | EQUAL -> ((l,e,r),true) *)
(*         (\* | GREATER -> let (n',b) = add_tree l e  in *\) *)
(*         (\*                    ((Node(n'),v,r),b) *\) *)
(*         (\* | LESS ->    let (n',b) = add_tree r e in *\) *)
(*         (\*                    ((l,v,Node(n')),b) *\) *)
(*         | GREATER -> let ((x,y,z),b) = add_tree l e  in *)
(*           ((Node(x,y,z),v,r),b) *)
(*        | LESS ->    let ((x,y,z),b) = add_tree r e in *)
(*           ((l,v,Node (x,y,z)),b) *)
(* ) *)


  let size s tr  = s

  type 'b folder = ((elem*'b)->'b) -> 'b -> key -> set -> 'b

  let rec add ((size,tr):set) (e:elem) = let
    ((l,v,r), b) = add_tree !tr e  in
    let node = splay (l,v,r)  (keyOf(e)) in
    let size' = if b then size else size+1
    in
    let _ = Printf.printf "Size %d" size' in
    ((size', ref (Node((l,v,r)))),b) and
  (* ((size', ref (Node((l,v,r)))),b) and *)

   add_tree  (t: tree) (e: elem) :node * bool =
    match t with
    |Empty -> ((Empty, e, Empty), false)
    | Node (l,v,r) ->
      (match compare (keyOf(v)) (keyOf(e)) with
       | EQUAL -> ((l,e,r),true)
       (* | GREATER -> let (n',b) = add_tree l e  in *)
       (*                    ((Node(n'),v,r),b) *)
       (* | LESS ->    let (n',b) = add_tree r e in *)
       (*                    ((l,v,Node(n')),b) *)
       | GREATER -> let ((x,y,z),b) = add_tree l e  in
         ((Node(x,y,z),v,r),b)
       | LESS ->    let ((x,y,z),b) = add_tree r e in
         ((l,v,Node (x,y,z)),b)
      )


  (* let add (size,tr) (e:elem) = let *)
  (*   ((l,v,r), b) = add_tree !tr e  in *)
  (*   let node = splay (l,v,r)  (keyOf(e)) in *)
  (*   let size' = if b then size else size+1 *)
  (*   in *)
  (*   let _ = Printf.printf "%d " size' in *)
  (*   ((size', ref ((l,v,r))),b) *)

  let rec fold_forward f b k (size,tr) =
  fold_forward_tree f b k (!tr)
  and fold_forward_tree (f: elem*'b->'b) (b:'b) (k:key) (t:tree) =
    match t with
  | Empty -> b
  | Node (l,v,r) ->
    (match compare (keyOf(v)) k  with
     |EQUAL -> fold_forward_tree f (f(v,b)) k r
             | LESS -> fold_forward_tree f b k r
             | GREATER -> let lv = fold_forward_tree f b k l in
             fold_forward_tree f (f(v,lv)) k r
    )     

  (* let first (size,tr) : elem option = raise (Failure "first: not implemented") *)

  (* let remove s e  = raise (Failure "remove: not implemented") *)

  (* let last s tr  = raise (Failure "last: not implemented") *)

  let fold_backward f b k s = raise (Failure "fold_backward: not implemented")


end

module I_Params = struct
  type key = int
  type elem = int
  let keyOf x = x
  let intOf x =x 
  let toString = string_of_int
  type order = LESS | EQUAL | GREATER
end


module IST = SplayTree( I_Params)

open IST

(* let ins_n  = *)
(*   let tree = (0, ref Empty) in *)
(*   let _i = add tree 1 in *)
(*   let j = add tree 2 in *)
(*   j *)
     
let rec ins_n n  =
  if n = 0 then empty() else
    let r = add (ins_n (n-1)) n in
    match r with
      | (x,_) -> x

