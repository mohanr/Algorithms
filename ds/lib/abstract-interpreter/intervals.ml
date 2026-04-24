module type ORDERED_FUNCTIONAL_SET = sig

  type interval = {
     pinf :  string;
     ninf : string
  }
  (* type set  *)                     (* I though we are dealing with sets *)
end

module type ORDERED_SET_PARAMS = sig
  type interval = {
     pinf :  string;
     ninf : string
  }
  val intervals : interval
end

module  IntervalDomain(Params : ORDERED_SET_PARAMS)
  : ORDERED_FUNCTIONAL_SET with type interval = Params.interval
                                   = struct
  type interval = Params.interval = {pinf :  string; ninf : string}

  let bot = ("bot","bot")       (* This version doesn't use proper types *)
  let top = (Params.intervals.ninf,Params.intervals.pinf)


    let norm av =
        match av with
          | av1, av0 -> if av1 == Params.intervals.ninf then bot   (* ..., -inf) *)
            else if av0 == Params.intervals.pinf then
              bot   (* (+inf, ... *)
            else
            if av0 > av1 then bot else av

    (* it helps to think of abstract elements as sets, with lte *)
    (* denoting set inclusion. So we're asking, is x included in y? *)
    let lte x y =
         (* bot is always less than everything else *)
         (* empty set {} is always included *)
        let x = norm x in
        let y = norm y in

        if Stdlib.compare x bot = 0 then true else
        if Stdlib.compare  y  bot = 0 then false
        else
        (* top is only lte *)
        (* top is all possible values, so it is only included in itself *)
        if Stdlib.compare x top = 0 then(
          (Stdlib.compare  y top = 0)
        ) else
         (* check if x is included in y *)
        match x,y with
          | (x0, x1),(y0, y1) when  x0 >= y0 && x1 <= y1 ->
            true
          | _ -> false
end


module I_Params = struct
  type interval = {
     pinf :  string;
     ninf : string
  }
  let intervals = { pinf = "+inf"; ninf = "-inf" }
end

module IST = IntervalDomain( I_Params)

open IST
