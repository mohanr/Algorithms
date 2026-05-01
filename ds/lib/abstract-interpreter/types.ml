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
