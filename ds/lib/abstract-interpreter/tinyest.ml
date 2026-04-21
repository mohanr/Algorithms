
type binaryOps =
    | Plus of char
    | Minus of char
    | Div of char
    | Neg of char
    | Mul of char
  [@@deriving show]

type comparisonOps =
    | Less of char
    | Great of char
    | StructEqu of string
    | Less_Eq of string
    | Great_Eq of string
    | Not_Eq of string
  [@@deriving show]

let operator c =
  match c with
     |'+' -> Plus c
     |'-' -> Minus c
     |'/' -> Div c
     | _ -> failwith "Wrong operator"

type expr =
  | BinOp of binaryOps * var * scalar
  | Seq of expr * expr
  | Assign of var * expr
  | If of expr * expr * expr
  | Input of var
  | BoolExpr of comparisonOps * var *  scalar
  | While of expr * expr
  | Skip
and scalar =
  | Scalar of int
and var = Var of char
[@@deriving show]


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
