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




