open Containers
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

    let int_size  = Sys.word_size - 1
    let umid = Array1.create int8_unsigned c_layout 1
    let new_range (m_ax : int) ranges =
      let  node =
        {
            by_start =  Node_vector.create();
            by_end =  Node_vector.create();
        } in
      let interval_tree = {

       nodes = Interval_vector.init 32 ( fun _ -> node ); (* TODO Pre-allocation value is hard-coded *)

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
                               Printf.printf "Id is **%d** " id  ;
                               List.iter (fun x -> Printf.printf "%d "   x ) range;
                               let mx = 1 in
                               let last = List.last range in
                               let first = List.first range in
                               let logc2 x = (Float.of_int x) |> Stdlib.Float.log2 |> Float.floor |> Int.of_float in
                               Array1.set umid 0 (Int.sub  (last  land ( int_size lsl (logc2(first lxor last))))  mx );
                               let u_mid = Array1.get umid 0 in
                               Printf.printf "mid is %d  \n " u_mid;
                               let n = CCVector.get interval_tree.nodes u_mid in
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
  let logc2 x = (Float.of_int x) |> Stdlib.Float.log2 |> Float.floor |> Int.of_float in
  let en_d = Int.sub ( logc2 (2 lsl (CCVector.length nodes)))  1 in
  (* Printf.printf "%d %d " (logc2 (2 lsl (CCVector.length nodes))) (2 lsl (CCVector.length nodes)) ; *)

  Seq.of_dispenser (fun _ ->
           let rec loop_while mid =
           if Int.(<) mid  en_d then
                try
                   let n = CCVector.get nodes mid in
                   let popped =
                     if point < mid then(
                         Printf.printf "point < mid  %d %d \n " point mid;
                        if CCVector.exists (fun ((start : int), _) ->
                              Int.(<=) start point) n.by_start then
                           CCVector.pop n.by_start
                        else None
                     ) else (
                        Printf.printf "point >= mid  %d %d \n " point mid;
                        if CCVector.exists (fun (_, en_d ) ->
                              Int.(<) point en_d ) n.by_start then
                           CCVector.pop n.by_start
                        else None
                    ) in
                   (match popped with
                    | Some (_,id) -> (* Printf.printf " ID %d " id; *)Some ("ID :" ^ Int.to_string id ^ "\n")
                    | None -> (* Printf.printf " None "; *)

                       let mid = (mid lor (Int.add mid  1)) land lnot (2 lsl (trailing_ones (Int64.of_int mid))) in
                       loop_while mid
                  )
                with Invalid_argument _mid-> Printf.printf "Invalid_Argument"; None
          else None
         in loop_while mid
         )

end
