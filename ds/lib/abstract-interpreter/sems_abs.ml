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


let rec evaluate_Expr expr m =
  match expr with
    | Const s -> s
    | Vars c -> get_scalar expr m
    | BinaryOps (op, left, right) ->
                  f_binop op (evaluate_Expr left m)
                             (evaluate_Expr right m)
    |_ -> failwith "evaluate_Expr"

let evaluate_BoolExpr expr  m : bool =
  match expr with
  | BoolExprs (op , left, right )->
    f_cmpop op ( get_scalar left m)
                         (get_scalar right m)
  |_ -> failwith "evaluate_BoolExpr "

let filter_memory b m  =
    (* TODO: why materialize this generator? *)
   let l =
    let rec loop_while_m m i acc   =
      if MemoryMap.cardinal m > i then
        let a = acc @ [(evaluate_BoolExpr b m = true)] in
        loop_while_m m (i + 1) a
      else
        acc
    in
    loop_while_m m 0 []
   in l
