open Batteries
open BatOrd
open BatRandom

let%expect_test "Test min/max range"=

  let range a b =
      let combined_list = List.append a b in
      let min_list = List.reduce( min Int.ord ) combined_list in
      let max_list = List.reduce( max Int.ord ) combined_list in
      List.iter (fun x -> Printf.printf "%d" x ) (List.of_enum(min_list--max_list) : int list);
      Printf.printf " %d %d" min_list max_list;
      [%expect {| 1234 1 4 |}]
  in range  (List.of_enum(1--2) : int list) (List.of_enum(3--4) : int list)

let%expect_test "Test generate range"=
    let () = self_init() in
    let r = int 32 in
    let rec data x =
      if x <= 10 then(
       let d = r |> fun a -> []@[a] |>  List.reduce ( max Int.ord ) |>
                                  fun m -> ((int (m - 1))--m) in
       List.iter (fun x -> Printf.printf "%d" x ) (List.of_enum d : int list) ;
       [%expect {|
         (* CR expect_test: Test ran multiple times with different test outputs *)
         ============================= Output 1 / 11 =============================
         9101112
         ============================= Output 2 / 11 =============================
         9101112
         ============================= Output 3 / 11 =============================
         3456789101112
         ============================= Output 4 / 11 =============================
         123456789101112
         ============================= Output 5 / 11 =============================
         0123456789101112
         ============================= Output 6 / 11 =============================
         789101112
         ============================= Output 7 / 11 =============================
         56789101112
         ============================= Output 8 / 11 =============================
         89101112
         ============================= Output 9 / 11 =============================
         101112
         ============================ Output 10 / 11 =============================
         123456789101112
         ============================ Output 11 / 11 =============================
         56789101112
         |}];
       data (x + 1)
     )else ()
    in data 0
    (* (max in 1usize..32)(max in Just(max), *)
    (*  ranges in vec(range(max), 10), point in 0..max) -> *)
    (*   (usize, Vec<Range<usize>>, usize) { *)
    (*         (max, ranges, point) *)

(*     proptest! { *)
(*         #[test] *)
(*         fn works_once((max, ranges, point) in data()) { *)
(*             let mut itree_output: Vec<usize> = *)
(*                 IntervalTree::new(max, ranges.iter().cloned().enumerate()) *)
(*                     .drain_containing(point) *)
(*                     .collect(); *)
(*             itree_output.sort(); *)

(*             let expected_output: Vec<usize> = ranges *)
(*                 .into_iter() *)
(*                 .enumerate() *)
(*                 .filter(|(_, range)| range.contains(&point)) *)
(*                 .map(|(id, _)| id) *)
(*                 .collect(); *)

(*             prop_assert_eq!(itree_output, expected_output); *)
(*         } *)
(*     } *)
(* } *)
