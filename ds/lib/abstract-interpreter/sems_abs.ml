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
    let rec loop_while_m m acc   =
      match m with
      |[] -> acc
      |hd :: tl ->
        let b = (evaluate_BoolExpr b hd = true) in
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
  let rec loop_while_m0 m  acc =
  match m0 with
      |[] -> acc
      |hd :: tl ->
             loop_while_m0 tl ( acc @ [loop_while_m hd ])
    in
    loop_while_m0 m0 []
    in
  let set1 =
 let rec loop_while_m1 m  acc =
  match m1 with
      |[] -> acc
      |hd :: tl ->
             loop_while_m1 tl ( acc @ [loop_while_m hd ])
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
    let out =
    let rec loop_while_m m0 acc =
      match m0 with
      |[] -> acc
      |hd :: tl ->
      let rec loop_while_l m i acc =
         if MemoryMap.cardinal m > i then

           let module MemoryMap_Copy = CCMap.Make(Memory) in
           let m_copy = MemoryMap_Copy.of_list (MemoryMap.to_list m) in
           let m_copy = MemoryMap_Copy.update var (Option.map (fun _ ->  value_lambda(m))) m_copy in

           loop_while_l m (i + 1) ( acc @ [m_copy])
         else loop_while_m tl acc
       in
       loop_while_l hd 0 []
     in
     loop_while_m m []
    in out
    in  match c with
          | Skip  -> m
          | Program p -> evaluate_Cmd c m
          | Assign (left ,right) -> update_memories (match left with
                                                     | Var c -> Char.escaped c)
                                     (fun m -> Const (evaluate_Expr right m))
          | Input i -> let n = Random.full_int 101  in (* could be anything, actually *)
                       update_memories (match i with
                                       | Var c -> Char.escaped c)
                                      (fun  m -> (Const (Scalar n)))
          | Seq (a,b) -> evaluate_Cmd a (evaluate_Cmd b m)
          | If (a,b,c)->
                  let then_memory = evaluate_Cmd b (filter_memory a m) in
                  let else_memory = evaluate_Cmd c (filter_memory a m) in
                    union_memories then_memory else_memory
          | While (a,b) ->
         (* L0 but we apply filter at the end *)
                 let out =
                   let rec loop_while_m m acc =
                     match m with
                     |[] -> acc
                     |hd::tl -> loop_while_m tl (acc @ [hd])
                   in
                   loop_while_m m [] (* copy all input states *)
                  in

         (* the next loop computes L1, L2, L3, .... *)
         (* identify those memories where condition is true *)

                 let pre_iter_memories = filter_memory c out in
                 let accum =
                   let rec loop_while_m m i acc =
                   if List.length m > i then
                       let after_iter_memories = evaluate_Cmd c pre_iter_memories in
                       let accum = union_memories acc after_iter_memories in
                       let pre_iter_memories = filter_memory c after_iter_memories in
                       loop_while_m m (i + 1) pre_iter_memories
                   else acc
                   in
                   loop_while_m pre_iter_memories 0 [] (* copy all input states *)

         (* This computes L0 U (L1 U L2...) and retains only those memory states where the loop has *)
         (* terminated. *)

         (* we have exited the loop, so only keep those memories where condition is false *)
                in
                let out = filter_memory c (union_memories out accum) in
                out
        | _ -> failwith "Don't know how to interpret {type(C).__name__}({C})"
