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
    Random.int 32

let generate_series r =
         let d = r + 1 |> fun a -> []@[a] (*  Needs a list here *)
                             |>  List.reduce ( max Int.ord ) |> (*  Find max*)
                                  fun m -> let r = int m in
                                           if r == 0 then
                                            ((r + 1)--m)  (*  Start < end, Start at 1 by adding 1 to random *)
                                           else
                                            (r --m) in (*  Start < end, Start at 1 by adding 1 to random *)
                                                                     (*  at the beginning *)
         d

let generate_range()  =
    let () = self_init() in
    let r = int 32 in
    let series =
    let rec data x acc=            (*  Generate 10 ranges*)
      if x <= 10 then(
       let acc = acc@[x,(generate_series r)] in
       data (x + 1) acc
     )else acc
    in data 0 []
    in series

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
         7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         ============================= Output 2 / 11 =============================
         1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         ============================= Output 3 / 11 =============================
         1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         ============================= Output 4 / 11 =============================
         6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         ============================= Output 5 / 11 =============================
         7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         ============================= Output 6 / 11 =============================
         6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         ============================= Output 7 / 11 =============================
         11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         ============================= Output 8 / 11 =============================
         18 19 20 21 22 23 24 25 26 27
         ============================= Output 9 / 11 =============================
         24 25 26 27
         ============================ Output 10 / 11 =============================
         1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         ============================ Output 11 / 11 =============================
         9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
         |}];
       data (x + 1)
     )else ()
    in data 0

let%expect_test "Find interval"=

      (*  TODO 32 is hard-coded*)
      let node = new_range 32 (generate_range()) in
      let p = generate_point() in
      Seq.iter print_string( fun() -> get_containing_data node p ());
      Printf.printf "Point searched is %d" p;
      [%expect {|
        Id is **0** 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         Id is **1** 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         Id is **2** 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         Id is **3** 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         Id is **4** 22 23 mid is 22
         Id is **5** 20 21 22 23 mid is 21
         Id is **6** 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         Id is **7** 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         Id is **8** 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         Id is **9** 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         Id is **10** 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 mid is 15
         point > mid  7 7
         ID :4
        point > mid  7 7
         ID :5
        point > mid  7 7
         ID :2
        point > mid  7 7
         ID :6
        point > mid  7 7
         ID :10
        point > mid  7 7
         ID :3
        point > mid  7 7
         ID :0
        point > mid  7 7
         ID :9
        point > mid  7 7
         ID :8
        point > mid  7 7
         point < mid  7 15
         ID :7
        point > mid  7 7
         point < mid  7 15
         ID :1
        point > mid  7 7
         point < mid  7 15
         point < mid  7 31
         Point searched is 7
        |}]
