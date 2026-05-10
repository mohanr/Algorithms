open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

type binaryOps =
    | Plus of char
    | Minus of char
    | Div of char
    | Neg of char
    | Mul of char
[@@deriving show ,compare, sexp]

type comparisonOps =
    | Less of char
    | Great of char
    | StructEqu of string
    | Less_Eq of string
    | Great_Eq of string
    | Not_Eq of string
[@@deriving show ,compare, sexp]

let operator c =
  match c with
     |'+' -> Plus c
     |'-' -> Minus c
     |'/' -> Div c
     | _ -> failwith "Wrong operator"
[@@deriving show ,compare, sexp]

(* Some types can be merged into 'expr*)
type expr =
  | Program of expr
  | BinOp of binaryOps * var * scalar
  | BinaryOps of binaryOps * expr * expr
  | ComparisonOps of comparisonOps * comparisonOps
  | Seq of expr * expr
  | Assign of var * expr
  | If of expr * expr * expr
  | Input of var
  | BoolExpr of comparisonOps * var *  scalar
  | BoolExprs of comparisonOps * expr*  expr
  | While of expr * expr
  | Vars  of char               (* Refactor*)
  | Const of scalar (* Refactor*)
  | Skip
and scalar =
  | Scalar of int
and var = Var of char
[@@deriving show ,compare, sexp]


 (* convenience function to turn a list into a sequence *)
let rec sequence l =
    match List.length l with
      | 0 -> failwith "Can't convert an empty list into a Seq"
      | 1 -> Seq ((List.nth l 0), Skip)
      | 2 ->  Seq ((List.nth l 0), (List.nth l 1))
      | _ -> Seq ((List.nth l 0),
                 sequence ( List.filteri
                              (fun i _ -> i >= 1 && i <=
                                                    (List.length l)) l ))
