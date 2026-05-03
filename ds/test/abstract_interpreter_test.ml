open Bloomfilter__.Tinyest
open Bloomfilter__.Abstractions
open Bloomfilter__.Types
open Bloomfilter__.Interval_point.IntervalPoint

let%expect_test _=
    let x = Var 'x' in
    let y = Var 'y' in
    let t = If ( BoolExpr((Less '<'), x, (Scalar 7)),
                           Assign (x, BinOp (( Neg  '-'), x, (Scalar 7))),
                           Assign (y, BinOp ((Minus '-'), x, (Scalar 7)))
               )
              in
 print_endline (show_expr t);
  [%expect {|
    (Tinyest.If (
       (Tinyest.BoolExpr ((Tinyest.Less '<'), (Tinyest.Var 'x'),
          (Tinyest.Scalar 7))),
       (Tinyest.Assign ((Tinyest.Var 'x'),
          (Tinyest.BinOp ((Tinyest.Neg '-'), (Tinyest.Var 'x'),
             (Tinyest.Scalar 7)))
          )),
       (Tinyest.Assign ((Tinyest.Var 'y'),
          (Tinyest.BinOp ((Tinyest.Minus '-'), (Tinyest.Var 'x'),
             (Tinyest.Scalar 7)))
          ))
       ))
    |}]

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
    (* assert not pinf < y *)
    (* assert ninf < x *)
    (* assert ninf < pinf *)
    (* assert ninf <= ninf *)
    (* assert y < pinf *)

    (* assert pinf > x *)
    (* assert y > x *)

    (* assert min(ninf, x) == ninf *)
    (* assert max(y, pinf) == pinf *)

let%expect_test _=

    let  open Bloomfilter__.Abstractions.I_Params in
    let m = [[('x', 25); ('y', 7); ('z', -12)];
         [('x', 28); ('y', -7); ('z', -11)];
         [('x', 20); ('y', 0); ('z', -10)];
         [('x', 35); ('y', 8); ('z', -9)]]
    in
  let _ = List.iter (fun (c,k) ->
    print_char  c;
    print_endline (show_interval k);
  ) (NRA.phi m) in
    [%expect {|
      x(Abstractions.I_Params.Tup ((Abstractions.I_Params.Int 20),
         (Abstractions.I_Params.Int 35)))
      y(Abstractions.I_Params.Tup ((Abstractions.I_Params.Int -7),
         (Abstractions.I_Params.Int 8)))
      z(Abstractions.I_Params.Tup ((Abstractions.I_Params.Int -12),
         (Abstractions.I_Params.Int -9)))
      |}]
