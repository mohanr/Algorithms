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
  let data()  =
    let d = int 32 |> fun a -> []@[a] |>  List.reduce ( max Int.ord ) |> fun m -> (0--m) in
    List.iter (fun x -> Printf.printf "%d" x ) ((List.of_enum d) : int list) ;
    [%expect {| 01234567891011 |}]
  in data()
