open Int32

let murmurhash chunks len seed =
  let c1 =  0xcc9e2d51l in
  let c2 =  0x1b873593l in
  let r1:int32 = (of_int 15) in
  let r2:int32 = (of_int 13) in
  let m = (of_int 5) in
  let n =  (of_string "0xe6546b64") in
  let h = ref zero in
  let k = ref zero in
  let l = div len (of_int 4) in
  h := seed;

 (* Printf.eprintf " %ld"  l; *)

 for i = 0 to (to_int l) - 1 do
  (* k := Bytes.get_int32_le chunks i; *)

  k := logor
       (logor
         (of_int (Bytes.get_uint8 chunks (i * 4)))
         (shift_left (of_int (Bytes.get_uint8 chunks ((i * 4)+ 1))) 8))
       (logor
         (shift_left (of_int (Bytes.get_uint8 chunks ((i * 4)+ 2))) 16)
         (shift_left (of_int (Bytes.get_uint8 chunks ((i * 4)+ 3))) 24));

  k := mul !k c1 ;
  k := logor (shift_left !k  (to_int r1))  (shift_right_logical !k  (Int32.to_int (Int32.sub (Int32.of_int 32)  r1)));
  k := mul !k c2;

  h := logxor !h !k;
  h := logor (shift_left !h  (to_int r2))  (shift_right_logical !h  (Int32.to_int (Int32.sub (Int32.of_int 32)  r2)));
  h := add ( mul !h m)  n;

  done;

let k = ref (of_int 0) in
let tail = to_int (mul l 4l) in
let l = (to_int len) - tail in

if l >= 3 then k := logxor !k (shift_left (of_int (Bytes.get_uint8 chunks (tail + 2))) 16);
if l >= 2 then k := logxor !k (shift_left (of_int (Bytes.get_uint8 chunks (tail + 1))) 8);
if l >= 1 then begin
  k := logxor !k (of_int (Bytes.get_uint8 chunks tail));

      k :=  mul !k c1;
      k := logxor (shift_left  !k (to_int r1))
          (shift_right_logical !k  (to_int (sub (of_int 32)  r1)));
      k :=  mul !k c2;
      h := logxor !h !k;
  end;

  h := logxor !h  len;

  h := logxor !h (shift_right_logical !h 16);
  h := mul !h  (of_string "0x85ebca6b");
  h := logxor !h (shift_right_logical !h 13);
  h := mul !h  (of_string "0xc2b2ae35");
  h := logxor !h (shift_right_logical !h 16);

  !h
