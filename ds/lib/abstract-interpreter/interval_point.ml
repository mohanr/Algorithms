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
        type elt = Ord.value
  type inter= | Int of int | Pinf | Ninf
   (* Repeated definition. Should belong in types.ml *)
   (* But interals.ml uses a certain pattern which may *)
   (* not reuse thie type from types.ml. Should be investigated *)

  let eq pt pt1 =
        (* this equates infinity, which should be okay *)
        match pt,pt1 with
        |inter1,inter2 ->
            inter1 = inter2

  let lt pt pt1 =
        match pt,pt1 with
        |Pinf,_ -> false
             (* +inf, -inf/F; +inf, n/F; +inf, +inf/F *)
        |Ninf,_ ->
             (* -inf, -inf/F; -inf, n/T; -inf, +inf/T *)
            pt1 != Ninf

        |_,Ninf -> false
             (* n, -inf/F *)

        |_,Pinf -> true
             (* n, +inf/F *)

        |_,_-> pt < pt1


    let le pt pt1 =
        match pt,pt1 with
        |Pinf,_ -> pt1 = pt  (* +inf == +inf *)
        |Ninf,_ ->
            true   (* -inf <= -inf, n, +inf *)
        |_,Ninf -> false
        |_,Pinf -> true
             (* _, +inf *)
        |_,_-> pt < pt1

    let gt pt pt1 =
        match pt,pt1 with
        |Pinf,_ ->
            pt1 != Pinf
        |Ninf,_ -> false
        |_,Ninf -> true
        |_,Pinf -> false
        |_,_-> pt > pt1



end
end
