
    let ltz = "[<= 0]"
    let gtz = "[>= 0]"
    let eqz = "[= 0]"
    let top = "TOP"
    let bot = "BOT"

    let phi v =
        (* Returns an abstract element for a concrete element *)
      match v with
      |  v when v = 0 ->
            eqz
      |  v when v > 0 ->
            gtz
      |  v when v < 0 ->
            ltz
      | _  ->
            failwith "Unknown value for signs abstraction "

    (* a best abstraction exists and is equal to phi *)
    let alpha = phi

    (* it helps to think of abstract elements as sets, with lte *)
    (* denoting set inclusion. So we're asking, is x included in y? *)
    let lte x y =
         (* bot is always less than everything else *)
         (* empty set {} is always included *)
      match x with
        |x when x = bot -> true

         (* top is only lte *)
         (* top is all possible values, so it is only included in itself *)
        |x when x = top ->
            if y != top then false
            else true

         (* eqz is the set {0}, which is included in all sets (>=0, <=0) except {} *)
        |x when x = eqz->
            if y = bot then false
            else true

        |x when x = ltz && x = gtz->
            if y = x then true else
            if y = top then true
            else false
        |_ -> false

            (* these sets are not included in {0} or {} or {>=0} [resp. {<=0}] *)
            (*  False *)

    let lub x y =
        (* Least upper bound, the smallest set that includes both x and y *)

        if lte x y then y  (* y includes x *)
        else if lte y x then x  (* x includes y *)
        else
        (* if incomparable, then we return T *)
        top

    let f_binop op left right =
      match op with
      | '+' ->
            lub left right
      | '*' ->
            if left != right then
                lub left right
            else if left = ltz then
                gtz   (* - * - = + *)
            else if left = gtz then
                gtz   (* + * + = + *)
            else
            failwith " NotImplementedError"
      | '-' ->
            if left = right then
                if left != eqz && left != bot then
                    top
                else
                left  (* {0} - {0} => {0}, {} - {} => {} *)
            else
                left    (* {+ve} - {-ve} => positive, {-ve} - {+ve} => {-ve} *)
      | _  ->
            failwith " NotImplementedError"

    let refine l r =
        if lte l r then l
        else if lte r l then r
        else top

    let f_cmpop op left c =
         (* (abst of c, op) : (variable's true domain, variables false domain) *)
       let open Types in
       let abs_results = AbsresultsMap.empty |>
          AbsresultsMap.add (eqz, (Char.escaped '<')) (ltz, gtz) |>
          AbsresultsMap.add (eqz, "<=") (ltz, gtz)|>
          AbsresultsMap.add  (eqz, Char.escaped '>') (gtz, ltz)|>
          AbsresultsMap.add  (eqz, ">=") (gtz, ltz)|>
          AbsresultsMap.add  (eqz, "!=") (top, eqz)|>

          AbsresultsMap.add  (gtz, Char.escaped '>') (gtz, top)|>
          AbsresultsMap.add  (gtz, Char.escaped '<') (top, gtz)|>
          AbsresultsMap.add  (gtz, "<=") (top, gtz)|>
          AbsresultsMap.add (gtz, ">=") (gtz, top)
        in
        let key = (c, op)
        in
        match AbsresultsMap.find_opt key  abs_results with
        |None -> failwith "NotFound"
        |Some v -> v
