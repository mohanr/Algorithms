open Bloomfilter__.Tinyest
open Bloomfilter__.Abstractions
open Bloomfilter__.Types
open Bloomfilter__.Interval_point.IntervalPoint
open Bloomfilter__Intervals
open Bloomfilter__Sems_abs
open Bloomfilter__Sems
(* Patrick Cousot's lecture notes — he invented abstract interpretation. Available at https://www.di.ens.fr/~cousot/ *)
(* "Principles of Abstract Interpretation" by Cousot (MIT Press, 2021) — the definitive textbook *)
(* CC 2012 tutorial by Rival & Yi — more accessible, search "introduction to abstract interpretation rival" *)
(* Francesco Logozzo's lectures on YouTube from OPLSS 2013 https://matt.might.net/articles/intro-static-analysis/ *)
(* https://www.youtube.com/channel/UCofC5zis7rPvXxWQRDnrTqA https://patiences.github.io/writing/static-analysis-and-abstract-interpretation*)
(* Isil Dillig's UT Austin slides — https://www.cs.utexas.edu/~isil/cs389L/AI-6up.pdf — *)
(*  covers exactly your widening operator definition on page 19: *)
(*   [a,b] ∇ [c,d] = [(c<a ? -∞ : a), (b<d ? +∞ : b)] — this is precisely your widen in IntervalDomain *)
(* Tel Aviv lecture notes — https://www.cs.tau.ac.il/~msagiv/courses/asv/ai_intro.pdf *)
(*  — walks through a worked example of widening step by step, very close to your k=1, k=2 output *)
(* let%expect_test _= *)
(*     let x = Var 'x' in *)
(*     let y = Var 'y' in *)
(*     let t = Program(If ( BoolExpr((Less '<'), x, (Scalar 7)), *)
(*                            Assign (x, BinOp (( Neg  '-'), x, (Scalar 7))), *)
(*                            Assign (y, BinOp ((Minus '-'), x, (Scalar 7))) *)
(*                )) *)
(*               in *)
(*  print_endline (show_expr t); *)
(*   [%expect {| *)
(*     (Tinyest.Program *)
(*        (Tinyest.If ( *)
(*           (Tinyest.BoolExpr ((Tinyest.Less '<'), (Tinyest.Var 'x'), *)
(*              (Tinyest.Scalar 7))), *)
(*           (Tinyest.Assign ((Tinyest.Var 'x'), *)
(*              (Tinyest.BinOp ((Tinyest.Neg '-'), (Tinyest.Var 'x'), *)
(*                 (Tinyest.Scalar 7))) *)
(*              )), *)
(*           (Tinyest.Assign ((Tinyest.Var 'y'), *)
(*              (Tinyest.BinOp ((Tinyest.Minus '-'), (Tinyest.Var 'x'), *)
(*                 (Tinyest.Scalar 7))) *)
(*              )) *)
(*           ))) *)
(*     |}] *)

module I_Params = struct
  type inter= Bloomfilter__.Types.inter
  type value = int
  let compare i i1 = Int.compare i i1
end

module IST = Make( I_Params)


let%expect_test _=
    let open IST in
    let _x = Int 5 in
    let y = Int 6 in
    let pinf = Pinf in
    let _ninf = Ninf in
    Printf.printf "%s" (Bool.to_string (lt pinf y));
    [%expect {| false |}]


module I_Params1 = struct
  include Bloomfilter__Types
  let top = (Ninf,Pinf)
  let bot = (Bot,Bot)
  let finite_height = false
end
module IST1 = IntervalDomain( I_Params1)
module NRA = NonRelationalAbstraction(IST1)

let abs_state_of_phi_map phi_map =
  MemoryMap.to_list phi_map
  |> List.map (fun (name, interval) -> (String.get name 0, interval))

let phi_abs_state memories =
  abs_state_of_phi_map (NRA.phi_map memories)

let%expect_test "Least Upper Bound"=
 let open Bloomfilter__Types in
 print_endline (show_interval  (IST1.lub  (Tup(Int (-1),Int (100)))
                                          (Tup(Int (9),Int (110)))));
    [%expect {|
      Types.Tup (new_left [(Types.Int -1)], new_right [(Types.Int 110)])
      (Types.Tup ((Types.Int -1), (Types.Int 110)))
      |}]

let%expect_test _=

    let m = [[('x', 25); ('y', 7); ('z', -12)];
         [('x', 28); ('y', -7); ('z', -11)];
         [('x', 20); ('y', 0); ('z', -10)];
         [('x', 35); ('y', 8); ('z', -9)]]
    in
  let _ = List.iter (fun (c,k) ->
    print_char  c;
    print_endline (show_interval k);
  ) (phi_abs_state m) in
    [%expect {|
       Types.Tup (new_left [(Types.Int 25)], new_right [(Types.Int 28)])
        Types.Tup (new_left [(Types.Int -7)], new_right [(Types.Int 7)])
        Types.Tup (new_left [(Types.Int -12)], new_right [(Types.Int -11)])
        Types.Tup (new_left [(Types.Int 20)], new_right [(Types.Int 28)])
        x includes y
       Types.Tup (new_left [(Types.Int -12)], new_right [(Types.Int -10)])
        Types.Tup (new_left [(Types.Int 20)], new_right [(Types.Int 35)])
        Types.Tup (new_left [(Types.Int -7)], new_right [(Types.Int 8)])
        Types.Tup (new_left [(Types.Int -12)], new_right [(Types.Int -9)])
       x(Types.Tup ((Types.Int 20), (Types.Int 35)))
      y(Types.Tup ((Types.Int -7), (Types.Int 8)))
      z(Types.Tup ((Types.Int -12), (Types.Int -9)))
      |}]

let%expect_test _=
let open Bloomfilter__.Stringinttuple in
let module S = Stdlib.Set.Make(StringIntTuple ) in
let s = S.(union (singleton ("s",Const (Scalar 1))) (singleton ("s",Const (Scalar 1)))) in
    List.iter ( fun (v, v1) -> Fmt.pr "%s %s" v (show_expr v1))
       (S.to_list s);
    [%expect {| s (Tinyest.Const (Tinyest.Scalar 1)) |}]

let%expect_test _=

    let open Bloomfilter__.Sems_abs in
    let x = Vars 'x' in
    let y = Vars 'y' in

    let binOp = BinaryOps ((Plus '+'), x, y) in
    let m = MemoryMap.empty |> MemoryMap.add (Char.escaped 'x') (Const (Scalar 5))
    |> MemoryMap.add (Char.escaped 'y') (Const (Scalar 6)) in
    let ex1 = evaluate_Expr binOp  m in
    match ex1 with
    | Scalar ex1 -> Printf.printf "%d" ex1;
    [%expect {| 11 |}]

let%expect_test _=
    (* TODO: actually put in asserts for testing. Right now, rely on visual inspection... *)
    let x = Vars 'x' in
    let y = Vars 'y' in
    let x_var = Var 'x' in
    let y_var = Var 'y' in

    let m1 = [('x', 5); ('y', 6)] in
    let m2 = [('x', 8); ('y', 7)] in

    let concrete_memories memory=
      let acc =
       let rec loop_while acc l =
         (match l with
         |[] -> acc
         | hd :: tl ->
           let acc1 =
            MemoryMap.fold (fun k v acc ->
            match k, v with
              | k, Const (Scalar s) -> acc @ [(String.get  k 0, s)]
              |_,_ -> failwith "concrete_memories"
             ) hd []
             in loop_while (acc @ [acc1]) tl
         )
         in loop_while [] memory
         in acc
    in
    let concrete_memory memory=
      List.fold_left
        (fun mem (name, value) ->
          MemoryMap.add (Char.escaped name) (Const (Scalar value)) mem)
        MemoryMap.empty memory
    in
    let m_in = [concrete_memory m1; concrete_memory m2] in

    let m_in_abs = phi_abs_state [m1; m2] in

    let s = Skip in
    let m_out = evaluate_Cmd s m_in in
    let m_out_abs = evaluate_Cmd_abs (Program Skip) m_in_abs (module NRA) in
    let _ =
    (match m_out with
    |[] -> Fmt.pr ""
    | hd :: _tl ->  MemoryMap.iter ( fun k v -> Fmt.pr "key [%s ]\n Value [ %s]\n"  k (show_expr v)) hd
    ) in
    Printf.printf "[%s Check ]" (Bool.to_string (NRA.included (concrete_memories m_out) m_out_abs));
    let pasgn = Assign (x_var, Const (Scalar 9)) in
    let pasgn_abs = Program(Assigns(x, Const (Scalar 9))) in
    let _m_out = evaluate_Cmd pasgn m_in in
    let _m_out_abs = evaluate_Cmd_abs pasgn_abs m_in_abs (module NRA) in

    let pinput = Input y_var in
    let pinput_abs = Program(Inputs(y)) in
    let _m_out = evaluate_Cmd pinput m_in in
    let _m_out_abs = evaluate_Cmd_abs pinput_abs m_in_abs (module NRA) in

    let pite = If(BoolExpr(Great '>', x_var, Scalar 7),
                  Assign (y_var, BinOp(Minus '-', x_var, Scalar 7)),
                  Assign (y_var, Const (Scalar 0))) in
    let pite_abs = Program(If(BoolExprs(Great '>', x, Const (Scalar 7)),
                              Assigns(y, BinaryOps(Minus '-', x, Const (Scalar 7))),
                              Assigns(y, BinaryOps(Minus '-', Const (Scalar 7), x))
                              )
                         ) in
    let _m_out = evaluate_Cmd pite m_in in
    let _m_out_abs = evaluate_Cmd_abs pite_abs m_in_abs (module NRA) in

    let ploop_abs = Program(While(BoolExprs(Less '<', x, Const (Scalar 7)),
                                  Seq(Assigns(y, BinaryOps(Minus '-', y, Const (Scalar 1))),
                                      Assigns(x, BinaryOps(Plus '+', x, Const (Scalar 1))))))
                    in
    let m_out_abs = evaluate_Cmd_abs ploop_abs m_in_abs (module NRA) in
    match m_out_abs with
    |[] -> Fmt.pr ""
    | (c, i) :: _tl -> Fmt.pr "%s %s" (Char.escaped c) (show_interval i);
    [%expect {|
       Types.Tup (new_left [(Types.Int 5)], new_right [(Types.Int 8)])
        Types.Tup (new_left [(Types.Int 6)], new_right [(Types.Int 7)])
       Program
       Skip
      key [x ]
       Value [ (Tinyest.Const (Tinyest.Scalar 5))]
      key [y ]
       Value [ (Tinyest.Const (Tinyest.Scalar 6))]
       Types.Tup (new_left [(Types.Int 5)], new_right [(Types.Int 8)])
        Types.Tup (new_left [(Types.Int 6)], new_right [(Types.Int 7)])
       [false Check ]Program
       Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 8)) ]
      [ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 9)) (Tinyest.Const (Tinyest.Scalar 9)) ]
      Assigns[ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Program
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 8)) ]
      [ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Program
       If
      then clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 8)) (Tinyest.Const (Tinyest.Scalar 8)) ]
      then clause[ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      else clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      else clause[ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 8)) (Tinyest.Const (Tinyest.Scalar 8)) ]
      [ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 8)) (Tinyest.Const (Tinyest.Scalar 8)) ]
      Assigns[ y ] [ (Tinyest.Const (Tinyest.Scalar 1)) (Tinyest.Const (Tinyest.Scalar 1)) ]
      x (Types.Tup ((Types.Int 8), (Types.Int 8)))Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      [ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns[ y ] [ (Tinyest.Const (Tinyest.Scalar 0)) (Tinyest.Const (Tinyest.Scalar 2)) ]
      x (Types.Tup ((Types.Int 5), (Types.Int 7))) Types.Tup (new_left [(Types.Int 5)], new_right [(Types.Int 8)])
        y includes x
      x (Types.Tup ((Types.Int 5), (Types.Int 8)))Program

      k=1
      equal=true finite_height false
      While clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      While clause[ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Seq
      Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      [ y ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      Assigns[ y ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      [ y ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns[ y ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
       x includes y
       (new_left [(Types.Int 5)], new_right [(Types.Int 8)]
        Types.Tup (new_left [(Types.Int 5)], new_right [(Types.Int 7)])
        (new_left [Types.Ninf], new_right [(Types.Int 7)]

      k=2
      equal=true finite_height false
      While clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      While clause[ y ] [ (Tinyest.Const (Tinyest.Scalar -4611686018427387904)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Seq
      Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      [ y ] [ (Tinyest.Const (Tinyest.Scalar -4611686018427387904)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      Assigns[ y ] [ (Tinyest.Const (Tinyest.Scalar -4611686018427387904)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 5)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      [ y ] [ (Tinyest.Const (Tinyest.Scalar -4611686018427387904)) (Tinyest.Const (Tinyest.Scalar 6)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 6)) (Tinyest.Const (Tinyest.Scalar 7)) ]
      Assigns[ y ] [ (Tinyest.Const (Tinyest.Scalar -4611686018427387904)) (Tinyest.Const (Tinyest.Scalar 6)) ]
       x includes y
       (new_left [(Types.Int 5)], new_right [(Types.Int 8)]
        x includes y
       (new_left [Types.Ninf], new_right [(Types.Int 7)]
       x (Types.Tup ((Types.Int 7), (Types.Int 8)))
      |}]

let%expect_test  "infinite_loop_abs"=
    let x = Vars 'x' in
    let top = Tup (Ninf,Pinf) in
    let m_in_abs =  [('x',top)] in
    let
    ploop3 = Program(Seq(Assigns(x, Const (Scalar 0)),
                         While(BoolExprs(Less_Eq "<=", x, Const (Scalar 100)),
                               If(BoolExprs(Great_Eq ">=", x, Const (Scalar 50)),
                                  Assigns(x, Const (Scalar 10)),
                                  Assigns(x, BinaryOps(Plus '+', x, Const (Scalar 1)))
                               ))
    )) in
    let m_out_abs = evaluate_Cmd_abs ploop3 m_in_abs (module NRA)in
    match m_out_abs with
    |[] -> Fmt.pr ""
    | (c, i) :: _tl -> Fmt.pr "%s Interval[  %s ]" (Char.escaped c) (show_interval i);
    [%expect {|
      Program
       Seq
      Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar -4611686018427387904)) (Tinyest.Const (Tinyest.Scalar 4611686018427387903)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 0)) (Tinyest.Const (Tinyest.Scalar 0)) ]

      k=1
      equal=true finite_height false
      While clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 0)) (Tinyest.Const (Tinyest.Scalar 0)) ]
      If
      then clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 4611686018427387902)) (Tinyest.Const (Tinyest.Scalar 4611686018427387902)) ]
      else clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 0)) (Tinyest.Const (Tinyest.Scalar 0)) ]
      Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 0)) (Tinyest.Const (Tinyest.Scalar 0)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 1)) (Tinyest.Const (Tinyest.Scalar 1)) ]
       y includes x
      x Types.Botx (Types.Tup ((Types.Int 1), (Types.Int 1)))x (Types.Tup ((Types.Int 1), (Types.Int 1))) Types.Tup (new_left [(Types.Int 0)], new_right [(Types.Int 1)])
        (new_left [(Types.Int 0)], new_right [Types.Pinf]

      k=2
      equal=true finite_height false
      While clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 0)) (Tinyest.Const (Tinyest.Scalar 100)) ]
      If
      then clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 50)) (Tinyest.Const (Tinyest.Scalar 100)) ]
      else clause[ x ] [ (Tinyest.Const (Tinyest.Scalar 0)) (Tinyest.Const (Tinyest.Scalar 49)) ]
      Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 50)) (Tinyest.Const (Tinyest.Scalar 100)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 10)) (Tinyest.Const (Tinyest.Scalar 10)) ]
      x (Types.Tup ((Types.Int 10), (Types.Int 10)))Assigns
       [ x ] [ (Tinyest.Const (Tinyest.Scalar 0)) (Tinyest.Const (Tinyest.Scalar 49)) ]
      Assigns[ x ] [ (Tinyest.Const (Tinyest.Scalar 1)) (Tinyest.Const (Tinyest.Scalar 50)) ]
      x (Types.Tup ((Types.Int 1), (Types.Int 50))) y includes x
      x (Types.Tup ((Types.Int 1), (Types.Int 50))) x includes y
       (new_left [(Types.Int 0)], new_right [Types.Pinf]
       x Interval[  (Types.Tup ((Types.Int 101), Types.Pinf)) ]
      |}]
