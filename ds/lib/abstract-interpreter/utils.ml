open Types
open Tinyest

let pinf_marker = Stdlib.max_int
let ninf_marker = Stdlib.min_int
let bot_marker = Stdlib.max_int - 1

let inter_to_expr inter =
  match inter with
  | Int i -> Const (Scalar i)
  | Pinf -> Const (Scalar pinf_marker)
  | Ninf -> Const (Scalar ninf_marker)

let interval_to_expr interval =
  match interval with
  | Bot -> Const (Scalar bot_marker)
  | _ -> failwith "What should I do ?"

let expr_to_inter expr =
  match expr with
  | Const (Scalar i) when i = pinf_marker -> Pinf
  | Const (Scalar i) when i = ninf_marker -> Ninf
  | Const (Scalar i) when i = bot_marker -> failwith "expr_to_inter: bot_marker"
  | Const (Scalar i) -> Int i
  | _ -> failwith "expr_to_inter"
