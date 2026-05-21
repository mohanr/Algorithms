open Containers
type inter= | Int of int | Pinf | Ninf
  [@@deriving show]
type interval  = |Bot |Tup of inter * inter
  [@@deriving show]
type mabs_t = Char.t * (inter * inter)
module  Intervalpoint = struct
    type t = inter*inter

    let compare t t1 =
      Stdlib.compare t t1
end

module IntervalpointMap = CCMap.Make(Intervalpoint)

type interval_points = (inter Option.t ) IntervalpointMap.t   (* Blocks for function *)

module  Memory = struct
    type t = string

    let compare s s1 =
      String.compare s s1
end

module MemoryMap = CCMap.Make(Memory)

module  Absresults = struct
    type t = string * string

    let compare s s1 =
      Stdlib.compare s s1
end

module AbsresultsMap = CCMap.Make(Absresults )

(* module type NonRelationalOperator= sig *)
(*   val phi : (char * int ) list list -> (char * interval) list *)
(* end *)


module  AbstractMemory = struct
    type t = string

    let compare s s1 =
      String.compare s s1
end
module AbstractMemoryMap = CCMap.Make(AbstractMemory )
