open Stdlib

type 'v values = Array
type 'k keys = Array


type ('k, 'v) leaf = 
  | LeafKeys of ('k) keys 
  | LeafValues of ('v) values 
  | LeafNext of  ('k, 'v) t
and ('k, 'v) t =
  | Leaf of ('k, 'v) leaf



type  ('k, 'v) node =
    | NodeKeys   of ('k) keys 
    | NodeChilds of ('k, 'v) children 
and
  ('k, 'v) children =   ('k, 'v) bTree Array.t
and ('k, 'v) bTree =
  | BTreeNode of ('k, 'v) node 
  | BTreeLeaf of ('k, 'v) leaf

let keys ls  =
 Array.of_list


let values ls  =
  Array.of_list

let leaves leafls =
  List.map (fun leaf -> BTreeLeaf leaf ) leafls
  |> Array.of_list

type 'a lazy_dlist_value =
  | Dnil
  | Dnode of 'a lazy_dlist_value * 'a * 'a lazy_dlist_value
and 'a lazy_dlist = 'a lazy_dlist_value Lazy.t
