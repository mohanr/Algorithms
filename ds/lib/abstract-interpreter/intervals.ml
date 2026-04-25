open Stdlib.Float

module type ORDERED_FUNCTIONAL_SET = sig

  type inter= | Int of int | Pinf | Ninf
  type interval  = |Bot |Tup of inter * inter
  (* type set  *)                     (* I though we are dealing with sets *)
end

module type ORDERED_SET_PARAMS = sig
  type inter= | Int of int | Pinf | Ninf
  type interval  = |Bot |Tup of inter * inter
end

module  IntervalDomain(Params : ORDERED_SET_PARAMS)
  : ORDERED_FUNCTIONAL_SET with  type inter=Params.inter and type interval=Params.interval
                                   = struct


  type inter = Params.inter= | Int of int | Pinf | Ninf
  type interval = Params.interval  = |Bot |Tup of inter * inter
  let top = (Params.Ninf,Params.Pinf)

    let norm av =
        match av with
          |  Params.Tup (l0,l1)  ->
           if l1 = Params.Ninf then  Params.Bot(* ..., -inf) *)
           else if l0 = Params.Pinf then Params.Bot(* (+inf, ... *)
           else if l0 > l1 then Params.Bot
           else av              (*  Should thid return ?*)
          | _ -> av

    let refine l r =
        let l = norm l in
        let r = norm r in

        match l,r with
          | Params.Bot,_->  r
          | _,Params.Bot->  l
          |  Params.Tup(l0,l1) , Params.Tup(r0,r1) ->
              match l0,r0,l1,r1 with
                |  Params.Int l00,Params.Int r00,Params.Int l11,Params.Int r11 ->
                   let new_start = Int.max l00 r00 in
                   let new_end = Int.min l11 r11 in
                   norm  (Params.Tup (Params.Int new_start, Params.Int new_end))
                | _,_,_,_ -> failwith "refine"


    (* it helps to think of abstract elements as sets, with lte *)
    (* denoting set inclusion. So we're asking, is x included in y? *)
    let lte x y =
         (* bot is always less than everything else *)
         (* empty set {} is always included *)
        let x = norm x in
        let y = norm y in

        (* top is only lte *)
        (* top is all possible values, so it is only included in itself *)
         (* check if x is included in y *)
        match x,y with
          | Params.Bot,_->  true
          | _,Params.Bot-> false
          | Tup (Params.Ninf,Params.Pinf),_ -> y = Tup ( Params.Ninf,Params.Pinf)
          |  Params.Tup(x0,x1) , Params.Tup(y0,y1) ->
              match x0,x1,y0,y1 with
                |  Params.Int x00,Params.Int x11,Params.Int y00,Params.Int y11
                     when  x00 >= y00 && x11 <= y11 ->
            true
          | _ -> false

    let lub x y =
        (* Least upper bound, the smallest set that includes both x and y *)
        let x = norm x in
        let y = norm y in

        if lte x y then y  (* y includes x *)
        else
        if lte y x then x  (* x includes y *)
        else
         (* note neither x nor y can be BOT at this point *)

        match x,y with
          |  Params.Tup(x0,x1) , Params.Tup(y0,y1) ->
             ( match x0,x1,y0,y1 with
                |  Params.Int x00,Params.Int x11,Params.Int y00,Params.Int y11 ->
                   let new_left = Int.min x00 y00 in
                   let new_right = Int.max x11 y11 in
                   Params.Tup (Params.Int new_left, Params.Int new_right)
                |   _->
                    failwith "lub error"
             )
          | (Tup (x0, x1), Bot) -> failwith "lub error"
          |  (Bot,_) -> failwith "lub error"

   let widen x y =

         (* assume x is previous and y is current *)

         (* compute union *)
        let u = lub x y in

        match u,x with
          | Params.Tup(u0, u1),Params.Tup(x0, x1)  ->
            if u0 = x0 then
             (* stationary left *)
            (u0, if u1 = x1 then u1 else Params.Pinf)
            else if u1 = x1 then
             (* stationary right *)
             ((if u0 = x0 then u0 else Params.Ninf), u1)
             else
            (u0,u1)
          | (Tup (u0, u1), Bot) -> failwith "widen error"
          |  (Bot, _) -> failwith "widen error"

    let f_binop op left right =
        let add_op x y =
          match x,y with
          |  Params.Tup(x0,x1) , Params.Tup(y0,y1) ->
             ( match x0,x1,y0,y1 with
                |  Params.Int x00,Params.Int x11,Params.Int y00,Params.Int y11 ->
              Params.Tup(Params.Int (x00 + y00),Params.Int( x11 + y11))
                |   _->
                    failwith "add_op error"
             )
          | (Tup (x0, x1), Bot) ->
                    failwith "add_op error"
          |  (Bot, _) ->
                    failwith "add_op error"
          in
        let sub_op x y =
          match x,y with
          |  Params.Tup(x0,x1) , Params.Tup(y0,y1) ->
             ( match x0,x1,y0,y1 with
                |  Params.Int x00,Params.Int x11,Params.Int y00,Params.Int y11 ->
              let a = x00 - y11  in  (* smallest of first interval - largest of second interval *)
              let b = x11 - y00 in   (* largest of first interval - smallest of second interval *)
              Params.Tup(Params.Int a,Params.Int b)
                |   _->
                    failwith "sub_op error"
             )
          | (Tup (x0, x1), Bot) ->
                    failwith "sub_op error"
          |  (Bot, _) ->
                    failwith "sub_op error"
          in
        let carry_out_op (op : Params.interval -> Params.interval -> Params.interval) left right =
          match left,right with
          |Params.Bot,_ |_,Params.Bot-> Params.Bot
          | _ ->
              op left right
        in
        let l = norm left in
        let r = norm right in

        if op = '+' then
            carry_out_op add_op  l r
        else if op = '-' then
            carry_out_op  sub_op l r
        else
            failwith "f_binop"



end


module I_Params = struct
  type inter= | Int of int | Pinf | Ninf
  type interval  = |Bot |Tup of inter * inter
end

module IST = IntervalDomain( I_Params)

open IST
