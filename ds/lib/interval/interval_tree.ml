open Containers
open Base
open Float
open Batteries
open Bigarray

module  Node_vector = CCVector

type node = {
    by_start : (int * int) Node_vector.vector;
 [@printer
        fun fmt map -> fprintf fmt "%a" (CCVector.pp  pp_by_start)]
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
      interval_tree.nodes
(*Reusable Stackoverflow answer which is based on *)
(* http://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightLinear *)
let trailing_ones bitfield =
let v = Int64.lognot bitfield in
let f = Int64.to_float  (Int64.logand v (Int64.neg v)) in (* cast the least significant bit in v to a float *)
let r =  Int64.sub ( Int64.shift_right (Int64.of_float f)  23) (Int64.of_string "0x7f") in
if Int64.equal r (Int64.neg 127L)  then
  32
else
  Int64.to_int r



 let get_containing_data nodes point =
  (* TODO Check if 'point' it is out of bounds *)
  let mid = point in
  let en_d = Int.sub (Int.of_float( log2 (Int.to_float (2 lsl (CCVector.length nodes)))))  1 in
  Seq.of_dispenser (fun _ -> Printf.printf "of_dispenser" ;
           let rec loop_while mid =
           if Int.(<) mid  en_d then
                try
                   let n = CCVector.get nodes mid in
                   let popped =
                        if CCVector.exists (fun ((start : int), _) -> Int.(<=) start point) n.by_start then
                           CCVector.pop n.by_start
                        else if CCVector.exists (fun (en_d, _) ->  Int.(<=) point en_d ) n.by_start then
                           CCVector.pop n.by_start
                        else None in
                   (match popped with
                    | Some (_,id) -> Printf.printf "%d" id;Some id
                    | None ->
                       let mid = (mid lor (Int.add mid  1)) land lnot (2 lsl (trailing_ones (Int64.of_int mid))) in
                       loop_while mid
                  )
                with Invalid_argument _mid-> Printf.printf "Invalid_Argument"; None
          else None
         in loop_while mid
         )

end
