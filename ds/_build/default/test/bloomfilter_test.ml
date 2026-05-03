open Bloomfilter

let string_to_print_list s =

  let str = s |> String.to_seq |> List.of_seq in
  let int_list = List.map int_of_char str in
  List.iter (fun c -> Printf.printf "%d\n" c) int_list


let string_to_int_list s =

  let str = s |> String.to_seq |> List.of_seq in
  let int_list = List.map int_of_char str in
  List.map (fun c -> Int32.of_int c) int_list

let%expect_test _=
  let hash = Bloomfilter.jenkins (string_to_int_list "Hello") in
  Printf.printf "%d\n" (Int32.to_int  hash);
  [%expect {| 1901914092 |}]


let%expect_test "hash" =
  let empty_list() : 'hf Bloomfilter.t = ref None in
  let l = empty_list() in
  let hf = Bloomfilter.insert_hashfunc l Bloomfilter.jenkins in
  let hash = hf.value (string_to_int_list "Hello") in
  Printf.printf "%d\n" (Int32.to_int  hash);
  [%expect {| 1901914092 |}]

let%expect_test "bitset" =
  let empty_list() : 'hf Bloomfilter.t = ref None in
  let l = empty_list() in
  let hf = Bloomfilter.insert_hashfunc l Bloomfilter.jenkins in
  let bit = Bloomfilter.set_indices (Bloomfilter.create_filter 9) "Hello" hf.value
  in
  Batteries.BitSet.print (BatInnerIO.output_channel stdout) bit ;
  [%expect {| 0000000000001000 |}]

let%expect_test "bitget" =
  let empty_list() : 'hf Bloomfilter.t = ref None in
  let l = empty_list() in
  let hf = Bloomfilter.insert_hashfunc l Bloomfilter.jenkins in
  let bit = Bloomfilter.get_indices (Bloomfilter.create_filter 9) "Hello" hf.value in
  Printf.printf "%s\n" (string_of_bool bit);
  [%expect {| true |}]

let%expect_test _=
  string_to_print_list "Hello";
  [%expect {|
    72
    101
    108
    108
    111 |}]
