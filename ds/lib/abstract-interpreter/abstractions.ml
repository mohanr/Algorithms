open Intervals
open Containers
open Types

module type ORDERED_SET_ABSTRACTIONS  = sig
    val get_interval_domain : unit -> (module ORDERED_FUNCTIONAL_SET)
    val union : (char * interval) list ->(char * interval) list ->(char * interval) list
    val widen : (char * interval)list ->  (char * interval)list ->(char * interval) list
    val phi : (char * int)list list ->  (char * interval) list
    val phi_map : (char * int)list list ->  interval MemoryMap.t
    val included : (char * int)list list ->  (char * interval) list -> bool
end

module NonRelationalAbstraction( Dom : ORDERED_FUNCTIONAL_SET)
  : ORDERED_SET_ABSTRACTIONS = struct
  (* The 'set' is to be explored further as we aren't using it explicitly. *)
  (* Only the 'union' function seems to be applicable here. *)


    module IntervalDomain = Dom

let get_interval_domain () =
  (module IntervalDomain :
    ORDERED_FUNCTIONAL_SET)

    let union m0 m1 =
      List.map
        (fun (x, hd) ->
          match Stdlib.List.assoc_opt x m1 with
          | Some hd1 -> (x, Dom.lub hd hd1)
          | None -> failwith "union error")
        m0
    (* construct an abstraction for a set of memories *)
    (* This return (char * Dom.interval list ) *)
    let phi m =
     let m_acc =
       let rec loop_while_accum m i m_accum =
        if i < List.length m  then (
        let m_abs =
         let mabs = CCArray.make (List.length (List.nth m i))  (' ',(Types.Tup(Types.Int 0,Types.Int 0))) in
         let rec loop_while_abs m i  mabs =
           if Array.length m > i then
             match Array.get m i with
             | x, y ->
            let a,b =  Dom.phi y in
            let _ = Array.set mabs i (x,Types.Tup (a, b)) in
            (* let _ = List.iter (fun (c,k) -> *)
            (*   print_char  c; *)
            (*   print_endline (Dom.show_interval k); *)
            (* ) (Array.to_list mabs) in *)
            loop_while_abs m (i + 1) mabs
           else
             mabs
          in loop_while_abs (Array.of_list (List.nth m i)) 0 mabs
          in
            let accum =
               if List.length m_accum = 0 then
                  Array.to_list m_abs
               else
                  union m_accum (Array.to_list m_abs)
               in
                   loop_while_accum  m (i + 1) accum
         ) else
            m_accum
         in loop_while_accum  m 0 []
        in
        (* also construct BOT TODO Investigate how this is used.*)
       m_acc

    let rec lte m0_abs m1_abs =
        match m0_abs, m1_abs with
          |[],[] -> true
          |hd :: tl,hd1 :: tl1 ->
            if not (Dom.lte hd hd1) then false else (lte tl tl1)
          |_,_ -> failwith "lte error"

    let widen m0 m1 =
      let acc =
       let rec loop_while acc m0 m1=
       match m0,m1 with
          |[],[] -> acc
          |(c, v) :: tl,(c1,v1) :: tl1 ->
            if  Char.(=) c c1 then
            let l, r = Dom.widen v v1 in
            loop_while (acc @ [(c,Types.Tup (l, r))]) tl tl1
            else failwith "widen error"
          |_,_ -> failwith "widen error"

     in loop_while [] m0 m1 in
     acc

    (* construct an abstraction for a set of memories *)
    (* This return (char * Dom.interval list ) *)
    let phi_map m =
      let m_map = MemoryMap.empty in
      List.fold_left (fun acc (c,k) ->
          MemoryMap.add  (Char.escaped c) k acc) m_map
       (phi m)


    (* convenience function *)
    let included m_conc m_abs =
        let interval_list = [] in
        let m_c_abs = phi m_conc in
        let i_list =
        List.fold_left (fun acc (c,k) -> k :: acc ) interval_list
        m_c_abs
        in
        lte i_list (List.map snd m_abs)

end
