open Tinyest
open Types
 (*An implementation of the concrete semantics, including an*)
(* interpreter*)

  (* map of variables (here str, instead of Var) -> values*)
  (*TODO: we could use var if we defined hash to be on the name of Var?*)

let f_binop op left right =
  match op,left,right with
     |Plus '+',Scalar left,Scalar right-> Scalar (left + right)
     |Minus '-',Scalar left,Scalar right -> Scalar (left - right)
     |Div '/',Scalar left,Scalar right -> Scalar (left / right)
     |Mul '*',Scalar left,Scalar right -> Scalar (left * right)
     |_ ->
        failwith "Unknown operator"

let f_cmpop op left right : bool =
  match op with
    | Less '<'-> left < right
    | Great '>'-> left > right
    | StructEqu "=="-> left == right
    | Less_Eq "<="-> left <= right
    | Great_Eq ">="-> left >= right
    | Not_Eq "!="-> left != right
     |_ ->
        failwith "Unknown comparison operator"

let get_scalar s m =
  match s with
    | Vars c -> (match MemoryMap.get (Char.escaped c) m with
                | Some v ->
                    (match v with
                    | Const s -> s
                    | _ -> failwith "Expecting a scalar"
                    )
                |None -> failwith "Memory Map failure"
                )
    |_ -> failwith "evaluate_Expr"

let get_scalar_var v m =
  match v with
  | Var c -> (match MemoryMap.get (Char.escaped c) m with
              | Some value ->
                  (match value with
                  | Const s -> s
                  | _ -> failwith "Expecting a scalar")
              | None -> failwith "Memory Map failure")


let rec evaluate_Expr expr m =
  match expr with
    | Const s -> s
    | Vars c -> get_scalar expr m
    | BinaryOps (op, left, right) ->
                  f_binop op (evaluate_Expr left m)
                             (evaluate_Expr right m)
    | BinOp (op, left, right) ->
                  f_binop op (get_scalar_var left m) right
    |_ -> failwith "evaluate_Expr"

let evaluate_BoolExpr expr  m : bool =
  match expr with
  | BoolExprs (op , left, right )->
    f_cmpop op ( evaluate_Expr left m)
                         (evaluate_Expr right m)
  | BoolExpr (op, left, right) ->
    f_cmpop op (get_scalar_var left m) right
  |_ -> failwith "evaluate_BoolExpr "

let filter_memory ?(res=true) b m  =
    (* TODO: why materialize this generator? *)
   let l =
    let rec loop_while_m m acc   =
      match m with
      |[] -> acc
      |hd :: tl ->
        let b = (evaluate_BoolExpr b hd = res) in
        let acc = if b then  hd::acc  else acc in
        loop_while_m tl acc
    in
    loop_while_m m []
   in l

let union_memories m0 m1 =
     (* this is, of course, ridiculous *)
  let open Stringinttuple in
  let open Core in              (* Janestreet Core *)

    (* convert everything to sets *)
  let loop_while_m m =
       MemoryMap.fold (fun k v acc -> Set.add acc (k,v)) m StringIntTupleSet.empty
  in
let set =
  let rec loop_while_m0 m acc =
    match m with          (* was m0 *)
    | [] -> acc
    | hd :: tl ->
        loop_while_m0 tl (acc @ [loop_while_m hd])
  in
  loop_while_m0 m0 []
in
let set1 =
  let rec loop_while_m1 m acc =
    match m with          (* was m1 *)
    | [] -> acc
    | hd :: tl ->
        loop_while_m1 tl (acc @ [loop_while_m hd])
  in
  loop_while_m1 m1 []
 in
 let s = StringIntTupleSetofSet.of_list  (set @set1) (* Assuming this is a 'union' *)
 in let l = List.map (Set.to_list s)  ~f:(fun m ->
           Set.fold m ~init:MemoryMap.empty ~f:( fun acc (k,v) ->
            MemoryMap.add k v acc ) )
 in l


let rec evaluate_Cmd c m =
    let open Containers in
    let open BatRandom in
    let update_memories var value_lambda =
      List.map (fun m ->
             MemoryMap.update var (Option.map (fun _ -> value_lambda m)) m
           ) m
    in  match c with
          | Skip  -> m
          | Program p -> evaluate_Cmd p m
          | Assign (left ,right) -> update_memories (match left with
                                                     | Var c -> Char.escaped c)
                                     (fun m -> Const (evaluate_Expr right m))
          | Input i -> let n = Random.full_int 101  in (* could be anything, actually *)
                       update_memories (match i with
                                       | Var c -> Char.escaped c)
                                      (fun  m -> (Const (Scalar n)))
          | Seq (a,b) -> evaluate_Cmd b (evaluate_Cmd a m)
          | If (a,b,c)->
                  let then_memory = evaluate_Cmd b (filter_memory a m) in
                  let else_memory = evaluate_Cmd c (filter_memory ~res:false a m) in
                    union_memories then_memory else_memory
          | While (a,b) ->
              let rec loop current accumulated =
                match filter_memory a current with
                | [] -> filter_memory ~res:false a accumulated
                | pre_iter ->
                  let after_iter = evaluate_Cmd b pre_iter in
                  loop after_iter (union_memories accumulated after_iter)
              in
              loop m m
        | _ -> failwith "Don't know how to interpret "
