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

let generate_point()  =
    let () = self_init() in
    int 32

let generate_series r =
         let d = r + 1 |> fun a -> []@[a] (*  Needs a list here *)
                             |>  List.reduce ( max Int.ord ) |> (*  Find max*)
                                  fun m -> ((int (m - 1))--m) in (*  Start < end, Start at 1 by adding 1 to random *)
                                                                     (*  at the beginning *)
         d

let generate_range()  =
    let () = self_init() in
    let r = int 32 in
    let series = [] in
    let rec data x =            (*  Generate 10 ranges*)
      if x <= 10 then(
       let _ = series@[x,(generate_series r)] in
       data (x + 1)
     )else series
    in data 0

let%expect_test "Test generate range"=
    let () = self_init() in
    let r = int 32 in
    let series = CCVector.create() in
    let rec data x =            (*  Generate 10 ranges*)
      if x <= 10 then(
       let () = CCVector.push series (generate_series r) in
       CCVector.iter (fun y  ->
                          (List.iteri (fun x i -> Printf.printf "[%d] %d " i  x )  (List.of_enum y : int list))
       ) series;

       Printf.printf "Iteration is %d" x;
       [%expect {|
         (* CR expect_test: Test ran multiple times with different test outputs *)
         ============================= Output 1 / 11 =============================
         [19] 0 [20] 1 [21] 2 [22] 3 [23] 4 [24] 5 [25] 6 [26] 7 [27] 8 [28] 9 [29] 10 [30] 11 [31] 12 Iteration is 0
         ============================= Output 2 / 11 =============================
         [19] 0 [20] 1 [21] 2 [22] 3 [23] 4 [24] 5 [25] 6 [26] 7 [27] 8 [28] 9 [29] 10 [30] 11 [31] 12 Iteration is 1
         ============================= Output 3 / 11 =============================
         [23] 0 [24] 1 [25] 2 [26] 3 [27] 4 [28] 5 [29] 6 [30] 7 [31] 8 Iteration is 2
         ============================= Output 4 / 11 =============================
         [13] 0 [14] 1 [15] 2 [16] 3 [17] 4 [18] 5 [19] 6 [20] 7 [21] 8 [22] 9 [23] 10 [24] 11 [25] 12 [26] 13 [27] 14 [28] 15 [29] 16 [30] 17 [31] 18 Iteration is 3
         ============================= Output 5 / 11 =============================
         [8] 0 [9] 1 [10] 2 [11] 3 [12] 4 [13] 5 [14] 6 [15] 7 [16] 8 [17] 9 [18] 10 [19] 11 [20] 12 [21] 13 [22] 14 [23] 15 [24] 16 [25] 17 [26] 18 [27] 19 [28] 20 [29] 21 [30] 22 [31] 23 Iteration is 4
         ============================= Output 6 / 11 =============================
         [25] 0 [26] 1 [27] 2 [28] 3 [29] 4 [30] 5 [31] 6 Iteration is 5
         ============================= Output 7 / 11 =============================
         [24] 0 [25] 1 [26] 2 [27] 3 [28] 4 [29] 5 [30] 6 [31] 7 Iteration is 6
         ============================= Output 8 / 11 =============================
         [4] 0 [5] 1 [6] 2 [7] 3 [8] 4 [9] 5 [10] 6 [11] 7 [12] 8 [13] 9 [14] 10 [15] 11 [16] 12 [17] 13 [18] 14 [19] 15 [20] 16 [21] 17 [22] 18 [23] 19 [24] 20 [25] 21 [26] 22 [27] 23 [28] 24 [29] 25 [30] 26 [31] 27 Iteration is 7
         ============================= Output 9 / 11 =============================
         [15] 0 [16] 1 [17] 2 [18] 3 [19] 4 [20] 5 [21] 6 [22] 7 [23] 8 [24] 9 [25] 10 [26] 11 [27] 12 [28] 13 [29] 14 [30] 15 [31] 16 Iteration is 8
         ============================ Output 10 / 11 =============================
         [28] 0 [29] 1 [30] 2 [31] 3 Iteration is 9
         ============================ Output 11 / 11 =============================
         [28] 0 [29] 1 [30] 2 [31] 3 Iteration is 10
         |}];
       data (x + 1)
     )else ()
    in data 0
