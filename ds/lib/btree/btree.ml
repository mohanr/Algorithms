open Stdlib

type 'v values = 'v array
type 'k keys = 'k array


type ('k, 'v) leaf= 
  | LeafKeys of ('k) keys 
  | LeafValues of ('v) values 
  | LeafNext of  ('k, 'v) t
and ('k, 'v) t =
  | Leaf of ('k, 'v) leaf
  | Empty


type  ('k, 'v) node =
      ('k) keys *  ('k, 'v) children
and
  ('k, 'v) children =   ('k, 'v) bTree Array.t
and ('k, 'v) bTree =
  | BTreeNode of ('k, 'v) node 
  | BTreeLeaf of ('k, 'v) leaf

let keys : 'b list -> 'b keys   =
  Array.of_list

let values ls  =
  Array.of_list

let leaves leafls:('k, 'v) bTree array
=
  List.map (fun leaf : ('k, 'v) bTree  -> leaf  ) leafls
  |> Array.of_list

let leafs k =
 BTreeLeaf (LeafKeys (k))

let btreenode keys children=
  BTreeNode (  keys, children)


let btree_example () =
  let leaf1 = leafs (keys [1; 2]) in
  let root = btreenode  (keys [3; 5])  (leaves [leaf1]) in
  root
