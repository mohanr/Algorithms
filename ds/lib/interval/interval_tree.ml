open Containers
open Batteries

module  Node_vector = CCVector

type node = {
    by_start : int64 Node_vector.vector;
    by_end : int64 Node_vector.vector
}
[@@deriving show]
module  Interval_vector = CCVector

type interval_tree = {
    nodes : node Interval_vector.vector;
}
[@@deriving show]


module IntervalTree = struct

    let new_range max ranges =
      let  node =
        {
            by_start =  Node_vector.create();
            by_end =  Node_vector.create();
        } in
      let interval_tree = {

       nodes = Interval_vector.create(); (* TODO Pre-allocation ? *)

      } in
      CCVector.push interval_tree.nodes node

      let ranger ranges  =
        let loop_while ra =

        let tuple_range = BatTuple.Tuple2.of_enum ra in
        BatEnum.iter (fun r ->
          match r with
            |(id, range) -> Printf.printf "ID %d" id
        )
        in
        loop_while ranges


end
