(* let rec rmq l r xs  = min (f l k') *)

open Batteries
open Stdlib

let rec append_lists l1 l2 =
  match l1 with
  | [] -> []
  | hd :: tl -> create_tuples hd l2 @ append_lists tl l2

and create_tuples e l =
  match l with
  | [] -> []
  | hd :: tl -> (e, hd) :: create_tuples e tl

let indx l i k = k * l + i

let enum_to_list enum =
  let k, i = enum in
  let kl = Enum.fold (fun acc x -> x :: acc) [] k |> List.rev in
  let il = Enum.fold (fun acc x -> x :: acc) [] i |> List.rev in
  append_lists kl il

(* Stackoverflow code *)
let log2 x = (Float.of_int x) |> Float.log2 |> Float.ceil |> Int.of_float

let tuples l  =
  let mk = log2 (List.length l) in
  let ps =
    let k = 0 --  mk
    and i = 0 -- (List.length l -1 ) in
    (k,i) in
    List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) (enum_to_list ps)

let  preprocess_a l mk =
       let ps =
         let k = 0 --  mk
         and i = 0 -- (List.length l -1 ) in
         (k,i) in

         let v = Array.make ((List.length l) * ( mk   + 1)) 0 in

           List.iter (fun (k, i) -> 

               let () = Printf.printf "[mk %d] [k %d] [i %d]\n" mk k i in
               let ind  = indx (List.length l )  in
               match k with
               | 0 ->
                      let index = ind i k in
                      let value = List.nth l (ind i 0) in
                      (* let () = Printf.printf "Value set is %d [k %d] [i %d]\n" value k i in *)

                      let v' = Array.set v index value in
                      Array.iter (fun elem -> Printf.printf " %d " elem) v

               | _ ->
                 let i' = i + (Batteries.Int.pow 2 ( k - 1)) in
                 let p1 = Array.get v ( ind i (k - 1) ) in
                 let p2 = Array.get v ( ind i' (k - 1)) in
                 (* let () = Printf.printf "p1 is %d p2 is %d [k %d] [i %d]\n" p1 p2 k i in *)

                 let v' = Array.set v (ind i k ) ( min p1 p2) in
                 Array.iter (fun elem -> Printf.printf " %d " elem) v
         ) (enum_to_list ps)


let  preprocess l mk =
       let ps =
         let k = 0 --  mk
         and i = 0 -- (List.length l -1 ) in
         Enum.cartesian_product k i in

         let v = BatVect.make ((List.length l) * ( mk   + 1)) 0 in

         Enum.iter (fun (k, i) ->

               let ind  = indx (List.length l )  in
               match k with
               | 0 ->
                      let index = ind i k in
                      let value = BatVect.get (BatVect.of_list l) (ind i 0) in
                      let () = Printf.printf "Value set is %d \n" value in
                      let v' = BatVect.set v index value in
                      BatVect.iter (fun elem -> Printf.printf " %d " elem) v'
               | _ ->
                 let i' = i + (Batteries.Int.pow 2 ( k - 1)) in
                 let p1 = BatVect.get v ( ind i (k - 1)) in
                 let p2 = BatVect.get v ( ind i' (k - 1)) in
                 let () = Printf.printf "p1 is %d p2 is %d \n" p1 p2 in
                 let v' = BatVect.set v (ind i k ) ( min p1 p2) in
                 BatVect.iter (fun elem -> Printf.printf " %d " elem) v'
             ) ps


                                                               
let rmq l =
  let mk = log2 (List.length l) in
  preprocess_a l mk

let ps mk l =
  let pr = 
  let k = 0 --  mk
  and i = 0 -- (List.length l -1 ) in
  Enum.cartesian_product k i in
  Enum.iter (fun (k, i) -> Printf.printf "k %d i %d\n " k i) pr
  (* k *)

  
let  () = let l = [9;8;7;6;5;4;3;2;1] in

  rmq l;

(* tuples l; *)

