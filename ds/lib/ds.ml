let jenkins ss : int32 =
  let rec hash_accu ( accu, l ):int32  =
    match l with
    | [] ->
      let hs = Int32.add accu (Int32.shift_left accu 3) in
      let hs1 = Int32.logxor hs (Int32.shift_right_logical hs 11) in
      Int32.add (Int32.shift_left hs1 15) hs1
    | hd :: tl ->
      let h = Int32.add accu hd in
      let accu = Int32.add h (Int32.shift_left h 10) in
      hash_accu (Int32.logxor accu (Int32.shift_right_logical accu 6), tl)
      (*  | [] -> *)
      (*           let hs = accu + (accu lsl 3) in *)
      (*           let hs1 = hs lxor (hs lsr 11) in *)
      (*           Int32.of_int (hs1 + (hs1 lsl 15)) *)
      (* | hd :: tl ->let h = accu + hd in *)
      (*              let accu = h + (h lsl 10) in *)
      (*   hash_accu ((accu lxor (accu lsr 6) ), tl) *)
  in
  hash_accu  ((Int32.of_int 0 ),ss)
    
type 'bloombits filter =
  { 
    bits : Batteries.BitSet.t
  }
type 'hf element =
  { value : 'hf
  ; mutable next : 'hf element option
  }
type 'hf  t = 'hf element option ref 
let create_filter initial_size : 'bloombits filter =
   let bits = Batteries.BitSet.create initial_size in
   { bits }

let insert_hashfunc t value =
  let new_hashfunc = { next = !t; value } in
  (match !t with
   | Some old_hashfunc  -> old_hashfunc.next
     <- Some new_hashfunc
   | None -> ());
  t := Some new_hashfunc;
  new_hashfunc

let string_to_int_list s =

  let str = s |> String.to_seq |> List.of_seq in
  let int_list = List.map int_of_char str in
  List.map (fun c -> Int32.of_int c) int_list

let set_indices filt  element hf =
  let length = Batteries.BitSet.capacity filt.bits in
  let hash = hf (string_to_int_list element) in
  let () = Batteries.BitSet.set filt.bits ((Int32.to_int hash) mod length) in
  filt.bits


let get_indices filt  element hf =
  let length = Batteries.BitSet.capacity filt.bits in
  let hash = hf (string_to_int_list element) in
  let () = Batteries.BitSet.set filt.bits ((Int32.to_int hash) mod length) in
  let bit = Batteries.BitSet.mem filt.bits ((Int32.to_int hash ) mod length) in
  bit
