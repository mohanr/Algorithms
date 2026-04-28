open Types
open Containers

module type ORDERED = sig
  type value
  val value : value
  val compare : value -> value  -> int
end


module type IntervalPt = sig
  type elt
  type inter
  val eq : inter -> inter -> bool
end
module IntervalPoint = struct

module Make (Ord : ORDERED ) : (IntervalPt with type elt := Ord.value) =
      struct

        type inter= Types.inter
        type elt = Ord.value
   (* Repeated definition. Should belong in types.ml *)
   (* But interals.ml uses a certain pattern which may *)
   (* not reuse thie type from types.ml. Should be investigated *)

  let eq pt pt1 =
        (* this equates infinity, which should be okay *)
        match pt,pt1 with
        |inter1,inter2 ->
            Stdlib.compare inter1  inter2 = 0

  let lt pt pt1 =
        match pt,pt1 with
        |Pinf,_ -> false
             (* +inf, -inf/F; +inf, n/F; +inf, +inf/F *)
        |Ninf,_ ->
             (* -inf, -inf/F; -inf, n/T; -inf, +inf/T *)
        if Stdlib.compare pt  pt1 <> 0 then true else false

        |_,Ninf -> false
             (* n, -inf/F *)

        |_,Pinf -> true
             (* n, +inf/F *)

        |_,_->if Stdlib.compare pt  pt1 < 0 then true else false


    let le pt pt1 =
        match pt,pt1 with
        |Pinf,_ -> if Stdlib.compare pt1 pt = 0 then true else false  (* +inf == +inf *)
        |Ninf,_ ->
            true   (* -inf <= -inf, n, +inf *)
        |_,Ninf -> false
        |_,Pinf -> true
             (* _, +inf *)
        |_,_->if Stdlib.compare pt  pt1 < 0 then true else false

    let gt pt pt1 =
        match pt,pt1 with
        |Pinf,_ ->
            Stdlib.compare pt1 Pinf = 0
        |Ninf,_ -> false
        |_,Ninf -> true
        |_,Pinf -> false
        |_,_-> if Stdlib.compare pt pt1 > 0 then true else false

    let add pt o =

    let m =
        IntervalpointMap.empty
        |> IntervalpointMap.add
         (Ninf, Ninf) (Some Ninf)
        |> IntervalpointMap.add
         (Ninf, Pinf) None  (* undefined -inf + +inf *)
        |> IntervalpointMap.add
         ( Pinf,Ninf) None  (* undefined: +inf + -inf*)
        |> IntervalpointMap.add
         ( Pinf,Pinf) (Some Pinf)
         in
         let res =
         (match( IntervalpointMap.find_opt (pt,o) m) with
         | Some index ->
           let result =  IntervalpointMap.get (pt,o) m in
           (match result with
           | Some v -> v
           | None -> failwith "Addition not defined "
           )
         | None ->
          (match pt,o with
            |Types.Int i,Types.Int i1-> Some (Types.Int (i + i1))
            |p,o when Stdlib.compare p Ninf <> 0 && Stdlib.compare p Pinf <> 0 -> Some o
                        (* n + -inf = -inf, n + +inf = +inf *)
            |_,_ -> Some pt
                        (* -inf - n = -inf, +inf - n = +inf *)
           ))
          in res




end
end
