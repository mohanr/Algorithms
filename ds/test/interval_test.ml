open Batteries
open BatOrd
open BatRandom
open Bloomfilter__Interval_tree.IntervalTree

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
                                  fun m ->
                                            ((int (m + 1))--m) in (*  Start < end, Start at 1 by adding 1 to random *)
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
                          (List.iter (fun x -> Printf.printf "%d "   x )  (List.of_enum y : int list))
       ) series;

       [%expect {|
         (* CR expect_test: Test ran multiple times with different test outputs *)
         ============================= Output 1 / 11 =============================
         30 31
         ============================= Output 2 / 11 =============================
         24 25 26 27 28 29 30 31
         ============================= Output 3 / 11 =============================
         29 30 31
         ============================= Output 4 / 11 =============================
         24 25 26 27 28 29 30 31
         ============================= Output 5 / 11 =============================
         2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
         ============================= Output 6 / 11 =============================
         27 28 29 30 31
         ============================= Output 7 / 11 =============================
         25 26 27 28 29 30 31
         ============================= Output 8 / 11 =============================
         15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
         ============================= Output 9 / 11 =============================
         28 29 30 31
         ============================ Output 10 / 11 =============================
         26 27 28 29 30 31
         ============================ Output 11 / 11 =============================
         26 27 28 29 30 31
         |}];
       data (x + 1)
     )else ()
    in data 0

let%expect_test "Find interval"=

      (*  TODO 32 is hard-coded*)
      let node = new_range 32 (generate_range()) in
      Seq.iter print_int( fun() -> get_containing_data node (generate_point()) ());
      [%expect {| of_dispenser |}]
