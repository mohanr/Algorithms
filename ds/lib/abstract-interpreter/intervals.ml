open Stdlib.Float
open Types
open Tinyest

module type ORDERED_FUNCTIONAL_SET = sig

  type inter = Types.inter
  type interval = Types.interval
  (* type set  *)                     (* I though we are dealing with sets *)
  val phi : int -> inter * inter
  val lte : interval -> interval -> bool
  val lub : interval -> interval -> interval
  val refine : interval -> interval -> interval
  val f_binop : binaryOps -> interval -> interval -> interval
  val f_cmpop : comparisonOps -> interval -> interval -> (inter * inter)*(inter * inter)
  val widen : interval -> interval ->  inter * inter
  val get_bot : unit -> (interval * interval)
  val get_finite_height : unit -> bool

end

module type ORDERED_SET_PARAMS = sig
  type inter = Types.inter
  type interval = Types.interval
  val top : (inter * inter)
  val bot : (interval * interval)
  val finite_height: bool

end

module  IntervalDomain(Params : ORDERED_SET_PARAMS)
  : ORDERED_FUNCTIONAL_SET with  type inter=Types.inter and type interval=Types.interval
                                   = struct


  type inter = Types.inter
  type interval = Types.interval

    let compare_inter a b =
      match a, b with
      | Ninf, Ninf | Pinf, Pinf -> 0
      | Ninf, _ -> -1
      | _, Ninf -> 1
      | Pinf, _ -> 1
      | _, Pinf -> -1
      | Int x, Int y -> Int.compare x y

    let min_inter a b =
      if compare_inter a b <= 0 then a else b

    let max_inter a b =
      if compare_inter a b >= 0 then a else b

    let add_inter a b =
      match a, b with
      | Ninf, Pinf | Pinf, Ninf -> failwith "add_inter error"
      | Ninf, _ | _, Ninf -> Ninf
      | Pinf, _ | _, Pinf -> Pinf
      | Int x, Int y -> Int (x + y)

    let sub_inter a b =
      match a, b with
      | Ninf, Ninf | Pinf, Pinf -> failwith "sub_inter error"
      | Ninf, _ | _, Pinf -> Ninf
      | Pinf, _ | _, Ninf -> Pinf
      | Int x, Int y -> Int (x - y)

    let get_top() = Params.top

    let get_bot() = Params.bot

    let get_finite_height() = false

    let phi v =
        (* Returns an abstract element for a concrete element *)
        Int v, Int v  (* this is the math interval [v, v] *)

    let norm av =
        match av with
          |  Types.Tup (l0,l1)  ->
           if l1 = Types.Ninf then  Types.Bot(* ..., -inf) *)
           else if l0 = Types.Pinf then Types.Bot(* (+inf, ... *)
           else if compare_inter l0 l1 > 0 then Types.Bot
           else av              (*  Should thid return ?*)
          | _ -> av

    let refine l r =
        let l = norm l in
        let r = norm r in

        match l,r with
          | Types.Bot,_->  Types.Bot(* r *)
          | _,Types.Bot->  Types.Bot(* l *)
          |  Types.Tup(l0,l1) , Types.Tup(r0,r1) ->
                   let new_start = max_inter l0 r0 in
                   let new_end = min_inter l1 r1 in
                   norm  (Types.Tup (new_start, new_end))


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
          | Types.Bot,_->  true
          | _,Types.Bot-> false
          | Tup (Types.Ninf,Types.Pinf),_ -> y = Tup ( Types.Ninf,Types.Pinf)
          |  Types.Tup(x0,x1) , Types.Tup(y0,y1) ->
              compare_inter x0 y0 >= 0 && compare_inter x1 y1 <= 0

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
          |  Types.Tup(x0,x1) , Types.Tup(y0,y1) ->
                   let new_left = min_inter x0 y0 in
                   let new_right = max_inter x1 y1 in
                   Types.Tup (new_left, new_right)
          | (Tup (x0, x1), Bot) -> failwith "lub error"
          |  (Bot,_) -> failwith "lub error"

   let widen x y =
         (* assume x is previous and y is current *)

         (* compute union *)
        let u = lub x y in

        match u,x with
          | Types.Tup(u0, u1),Types.Tup(x0, x1)  ->
            let new_l = if compare_inter u0 x0 < 0 then Types.Ninf else u0 in
            let new_r = if compare_inter u1 x1 > 0 then Types.Pinf else u1 in
            (new_l, new_r)
          | (Tup (u0, u1), Bot) -> failwith "widen error"
          |  (Bot, _) -> failwith "widen error"

    let f_binop op left right =
        let add_op x y =
          match x,y with
          |  Types.Tup(x0,x1) , Types.Tup(y0,y1) ->
              Types.Tup(add_inter x0 y0, add_inter x1 y1)
          | (Tup (x0, x1), Bot) ->
                    failwith "add_op error"
          |  (Bot, _) ->
                    failwith "add_op error"
          in
        let sub_op x y =
          match x,y with
          |  Types.Tup(x0,x1) , Types.Tup(y0,y1) ->
              let a = sub_inter x0 y1  in  (* smallest of first interval - largest of second interval *)
              let b = sub_inter x1 y0 in   (* largest of first interval - smallest of second interval *)
              Types.Tup(a, b)
          | (Tup (x0, x1), Bot) ->
                    failwith "sub_op error"
          |  (Bot, _) ->
                    failwith "sub_op error"
          in
        let carry_out_op (op : Types.interval -> Types.interval -> Types.interval) left right =
          match left,right with
          |Types.Bot,_ |_,Types.Bot-> Types.Bot
          | _ ->
              op left right
        in
        let l = norm left in
        let r = norm right in

        let open Tinyest in
        match op with
          |Plus '+' ->
            carry_out_op add_op  l r
          |Minus '-' ->
            carry_out_op  sub_op l r
          | _ ->
            failwith "f_binop"

    let f_cmpop op left c =
        let l = norm left in
        let c = norm c in

         (* assume integers *)
        let open Tinyest in
        match op with
          |Less '<' ->
            (match c with
              |  Types.Tup(x0,x1)  ->
                 (match x0 with
                   | Int i ->
                   (Ninf, Int (i - 1)), (Int i, Pinf)
                   | _ -> failwith "Wrong type in f_cmpop"
                 )
              | _ -> failwith "Wrong type in f_cmpop"
            )
          |Less_Eq "<=" ->
            (match c with
              |  Types.Tup(x0,x1)  ->
                 (match x0 with
                   | Int i ->
                   (Ninf, Int i), (Int (i + 1), Pinf)
                   | _ -> failwith "Wrong type in f_cmpop"
                 )
              | _ -> failwith "Wrong type in f_cmpop"
            )
          |Great '>' ->
            (match c with
              |  Types.Tup(x0,x1)  ->
                 (match x0 with
                   | Int i ->
                      (Int (i + 1), Pinf), (Ninf, Int i)
                   | _ -> failwith "Wrong type in f_cmpop"
                 )
              | _ -> failwith "Wrong type in f_cmpop"
            )
          |Great_Eq ">=" ->
            (match c with
              |  Types.Tup(x0,x1)  ->
                 (match x0 with
                   | Int i ->
                     (Int i, Pinf), (Ninf, Int (i - 1))
                   | _ -> failwith "Wrong type in f_cmpop"
                 )
              | _ -> failwith "Wrong type in f_cmpop"
            )
          |_ -> failwith "NotImplementedError "



end


module I_Params= struct
  type inter = Types.inter
  type interval = Types.interval
  let top = (Ninf,Pinf)
  let bot = (Types.Bot,Types.Bot)
  let finite_height = false
end

module IST = IntervalDomain( I_Params)
