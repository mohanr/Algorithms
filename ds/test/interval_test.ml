open Batteries
open BatOrd

let%expect_test "Test range"=

  let range a b =
      let combined_list = List.append a b in
      let min_list = List.reduce( min Int.ord ) combined_list in
      let max_list = List.reduce( max Int.ord ) combined_list in
      List.iter (fun x -> Printf.printf "%d" x ) (List.of_enum(min_list--max_list) : int list);
      Printf.printf " %d %d" min_list max_list;
      [%expect {| 12341 4 |}]
  in range  (List.of_enum(1--2) : int list) (List.of_enum(3--4) : int list)
