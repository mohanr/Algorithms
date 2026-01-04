open Containers
open Base
open Float
open Batteries

module  Node_vector = CCVector

type node = {
    by_start : (int * int) Node_vector.vector;
    by_end : (int * int) Node_vector.vector
}
[@@deriving show]
module  Interval_vector = CCVector

type interval_tree = {
    nodes : node Interval_vector.vector;
}
[@@deriving show]


module IntervalTree = struct

    let new_range (m_ax : int) ranges =
      let  node =
        {
            by_start =  Node_vector.create();
            by_end =  Node_vector.create();
        } in
      let interval_tree = {

       nodes = Interval_vector.create(); (* TODO Pre-allocation ? *)

      } in
      let () = CCVector.push interval_tree.nodes node in
      let () =
      List.iter (fun r ->
          (match r with
          | id,ranger -> let range : int list = List.of_enum ranger in
                            if Int.(>) (List.nth  (List.rev range) 0)  m_ax  then(
                               failwith "range out of bounds"
                            )
                            else(
                               let mx : int = 1 in
                               let last: int = List.last range in
                               let first: int = List.first range in
                               let mid : int = Int.sub (Int.of_float( log2 (Int.to_float (last  land ( mx lsl (first lor last))))))  mx in
                               let n = CCVector.get interval_tree.nodes mid in
                               let () = CCVector.push n.by_start (first, id) in
                               CCVector.push n.by_end (last, id);
                           )
        )) ranges in
      let () = CCVector.iter (fun vec ->
                                CCVector.sort' ( fun (start, _) (start1, _) ->
                                                 Int.compare start start1
                                               ) vec.by_start) interval_tree.nodes in (*  Descending*)
      let () = CCVector.iter (fun vec ->
                                CCVector.sort' ( fun (_,en_d) ( _,en_d1) ->
                                                 Int.compare en_d en_d1
                                               ) vec.by_end) interval_tree.nodes in (*  Descending*)
        ()

end
