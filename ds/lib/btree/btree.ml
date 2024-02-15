open Stdlib

type 'v values = Array
type 'k keys = Array


type ('k, 'v) leaf = 
  | LeafKeys of ('k) keys 
  | LeafValues of ('v) values 
  | LeafNext of  ('k, 'v) t
and ('k, 'v) t =
    | Leaf of ('k, 'v) leaf

