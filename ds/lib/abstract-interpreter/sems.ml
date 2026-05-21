open Sems_abs
open Types
open Tinyest
open Intervals
open Abstractions

(* TODO Move to types.ml *)
let get_scalar s m =
  match s with
    |  c -> (match (MemoryMap.get (Char.escaped c) m) with
                | Some (v,v1) ->
                    (match v,v1 with
                    | Const _, Const _ ->
                      (Utils.expr_to_inter v, Utils.expr_to_inter v1)
                    | _ -> failwith "Expecting a scalar"
                    )
                |None -> failwith "AbstractMemoryMap failure"
                )

let rec evaluate_Expr_abs expr m  (vabs : (module ORDERED_FUNCTIONAL_SET))=

  Printf.printf "evaluate_Expr_abs \n%!";
  let  module V  =(val vabs  : ORDERED_FUNCTIONAL_SET ) in
  match expr with
    | Const s  -> (match s with | Scalar s -> let s, s1 = V.phi s in Tup (s, s1))
    | Vars c ->  let s, s1 = get_scalar c m in  Tup (s, s1)
    | BinaryOps (op, left, right) ->
                 let interval =
                  V.f_binop op (evaluate_Expr_abs left m vabs)
                             (evaluate_Expr_abs right m vabs) in
                      interval
    |_ -> failwith "evaluate_Expr_abs"




let evaluate_BoolExpr_abs expr m  (vabs : (module ORDERED_FUNCTIONAL_SET))=
  let  module V  =(val vabs  : ORDERED_FUNCTIONAL_SET) in
  match expr with
  | BoolExprs (op , left, right )->
     let s, s1 =
      ( get_scalar (match left with | Vars l -> l | _ ->failwith "get_scalar") m) in
    V.f_cmpop op ( Tup (s, s1))
       (match right with | Const (Scalar  i) ->
          let s, s1 = V.phi i in (Tup (s, s1)) | _ ->failwith "f_cmpop")
       | _ ->failwith "f_cmpop"

let get_bot  (vabs : (module ORDERED_FUNCTIONAL_SET))=

        let  module V  =(val vabs  : ORDERED_FUNCTIONAL_SET) in
        V.get_bot()

let filter_memory_abs b m_abs (vabs : (module ORDERED_FUNCTIONAL_SET)) =

  Printf.printf "filter_memory_abs\n%!";
  let open Utils in
  let  module V  =(val vabs  : ORDERED_FUNCTIONAL_SET) in
  let true_abs, false_abs = evaluate_BoolExpr_abs b m_abs vabs in
  let bot = (match get_bot vabs with (* It is a tuple *)
                     | b, _-> b ) in
  match b with
  | BoolExprs (op , left, right )->
    let var_abs =  get_scalar (match left with | Vars l -> l | _ ->failwith "get_scalar") m_abs in
    let true_abs = V.refine  (let s, s1 = var_abs in Tup (s, s1)) (let s, s1 = true_abs in Tup (s, s1)) in
    let m_abs_true =
    if (true_abs <> bot) then(
         (* may enter true part *)
      (match true_abs with
      | Tup (x,x1)->
        MemoryMap.update
             (match left with | Vars l -> (Char.escaped l) | _ ->failwith "get_scalar" )
             (Option.map (fun _ ->((inter_to_expr x),(inter_to_expr x1)))) m_abs
      | _ -> failwith "Expected interval"
      )
    )
    else(
        let bot,bot1 =match get_bot vabs with (* It is a tuple *)
                     | b, b1-> interval_to_expr b,interval_to_expr b1 in
        MemoryMap.map (fun m ->  (bot,bot1)) m_abs
    ) in
      let false_abs =
      (match var_abs, false_abs  with
      | (x,x1),(y,y1)->
           V.refine ( Tup (x,x1)) (Tup (y,y1))
      ) in

    let m_abs_false =
    if false_abs <> bot then(
         (* may enter false part *)
      (match false_abs with
      | Tup (x,x1)->
        MemoryMap.update
             (match left with | Vars l -> (Char.escaped l) | _ ->failwith "get_scalar" )
             (Option.map (fun _ ->((inter_to_expr x),(inter_to_expr x1)))) m_abs
      | _ -> failwith "Expected interval"
      )
    )
    else(
        let bot,bot1 =match get_bot vabs with (* It is a tuple *)
                     | b, b1-> interval_to_expr b,interval_to_expr b1 in
        MemoryMap.map (fun m ->  (bot,bot1)) m_abs
    ) in

    (m_abs_true, m_abs_false)
  | _ -> failwith "filter_memory_abs"

let abs_iter f_abs m_abs (abstraction  : (module ORDERED_SET_ABSTRACTIONS)) =
    let  module V  =(val abstraction  : ORDERED_SET_ABSTRACTIONS) in
    let module V_abs = (val  V.get_interval_domain()
     : ORDERED_FUNCTIONAL_SET
    ) in
    let result =
    let rec loop_while r k =
        Printf.printf "k=%d\n" k;
        if k > 5  then
          r
        else
        let t = r in
        Printf.printf "equal=%b\n" (r = t);
          let r =
            if V_abs.get_finite_height() then(
                 V.union r (f_abs r)
            )
            else (
                 V.widen r (f_abs r )
            ) in
          if r = t then
            t
          else
            loop_while r (k + 1)
    in loop_while m_abs 1
    in result

let get_specific_hack_bot() =
  [('x', Types.Bot)]
 (* M_abs is the abstract set of memory states *)
let evaluate_Cmd_abs c m_abs ( abstraction : (module ORDERED_SET_ABSTRACTIONS))=
  Printf.printf "evaluate_Cmd_abs called\n%!";
    let  module A  =(val abstraction : ORDERED_SET_ABSTRACTIONS) in
    let vabs = A.get_interval_domain() in
    let expr_pair_of_interval interval =
      match interval with
      | Tup (x, x1) -> (Utils.inter_to_expr x, Utils.inter_to_expr x1)
      | Bot ->
        let bot_expr = Utils.interval_to_expr Bot in
        (bot_expr, bot_expr)
    in
    let interval_of_expr_pair (left, right) =
      let bot_expr = Utils.interval_to_expr Bot in
      match left, right with
      | l, r when l = bot_expr && r = bot_expr -> Bot
      | Const _, Const _ -> Tup (Utils.expr_to_inter left, Utils.expr_to_inter right)
      | _ -> failwith "interval_of_expr_pair"
    in
    let to_memory_map abs_state =
      List.fold_left
        (fun acc (var, interval) ->
          MemoryMap.add (Char.escaped var) (expr_pair_of_interval interval) acc)
        MemoryMap.empty abs_state
    in
    let of_memory_map mem =
      MemoryMap.to_list mem
      |> List.map (fun (var, value) -> (String.get var 0, interval_of_expr_pair value))
    in
    let rec eval_list cmd abs_state =
      Printf.printf "eval_list: %s\n%!" (show_expr cmd);
      let update_abs_memories mem var value_lambda =
        MemoryMap.update var (Option.map (fun _ -> value_lambda mem)) mem
      in
     (* C[BOT] -> BOT *)
      if List.exists (fun (_, interval) -> interval = Bot) abs_state then
        abs_state
      else
        match cmd with
          | Skip  -> abs_state
          | Program p -> eval_list p abs_state

          | Assigns (left ,right) ->
              let mem = to_memory_map abs_state in
              let mem =
                update_abs_memories mem
                                       (match left with
                                         | Vars c -> Char.escaped c
                                         | _ -> failwith "update_abs_memories")
                                       (fun m -> expr_pair_of_interval (evaluate_Expr_abs right m vabs))
              in
              of_memory_map mem
          | Inputs i ->
              let mem = to_memory_map abs_state in
              let mem =
                update_abs_memories mem
                          (match i with
                           | Vars c -> Char.escaped c
                           | _ -> failwith "update_abs_memories")
                          (fun m -> expr_pair_of_interval (evaluate_Expr_abs i m vabs))
              in
              of_memory_map mem
          | Seq (a,b) -> eval_list b (eval_list a abs_state)
          | If (a,b,c)->
                  let then_memory, else_memory = filter_memory_abs a (to_memory_map abs_state) vabs in
                  let then_memory = eval_list b (of_memory_map then_memory) in
                  let else_memory = eval_list c (of_memory_map else_memory) in
                  let ite_memory = A.union then_memory else_memory in
                  ite_memory

          | While (a,b) ->
              let f_abs mm_abs =
                Printf.printf "f_abs\n%!";
                let pre_memory, _ = filter_memory_abs a (to_memory_map mm_abs) vabs in
                eval_list b (of_memory_map pre_memory)
              in
              let _, out = filter_memory_abs a (to_memory_map (abs_iter f_abs abs_state abstraction)) vabs in
              of_memory_map out
          | _ ->
              failwith "Don't know how to interpret "
    in
    eval_list c m_abs
