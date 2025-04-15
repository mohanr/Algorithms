
   type 'bloombits filter =
   {
     bits : Batteries.BitSet.t
   }

   let  getline (h:int32)  (num_lines:int32) : int32 =
         Int32.rem h  num_lines

   let add_hash filt (h:int32)  (num_lines:int32) num_probes  (log2_cacheline_bytes:int) =


        let log2_cacheline_bits = Int32.add (Int32.of_int log2_cacheline_bytes)  (Int32.of_int 3) in
        let  base_offset = Int32.shift_left (getline h num_lines)  log2_cacheline_bytes in
        let delta = Int32.logor (Int32.shift_right_logical h  17)
                    (Int32.shift_left h  15) in

        let rec probe i  numprobes base_offset =
            let log2c = Int32.shift_left (Int32.of_int 1) (Int32.to_int log2_cacheline_bits)   in
            let bitpos = Int32.sub  log2c  (Int32.of_int 1) in
            let byteindex = (Int32.add base_offset  (Int32.div bitpos  (Int32.of_int 8))) in
            let () = Batteries.BitSet.set filt.bits (Int32.to_int (Int32.logor byteindex (Int32.shift_left (Int32.rem bitpos  (Int32.of_int 8)) 1))) in
            if i < num_probes then
              probe (i + 1) numprobes base_offset
            else
              (Int32.add h delta)
        in  probe 0 num_probes base_offset

        (* Recommended test to just check the effect of logical shift on int32. *)
        (* int64 doesn't seem to need it *)

        (* let  high : int32 = 2100000000l in *)
        (* let  low : int32 = 2000000000l in *)
        (* Printf.printf "mid using >>> 1 = %ld mid using / 2   = %ld" *)
        (*   (Int32.shift_right_logical (Int32.add low  high) 1) (Int32.div (Int32.add low high)  (Int32.of_int 2)) ; *)


    let hash_maymatch_prepared filt h  num_probes offset log2_cacheline_bytes =
        let log2_cacheline_bits = Int32.add (Int32.of_int log2_cacheline_bytes)  (Int32.of_int 3) in
        let delta = Int32.logor (Int32.shift_right_logical h  17)
                    (Int32.shift_left h  15) in

        let rec probe h i  numprobes base_offset =
            let log2c = Int32.shift_left (Int32.of_int 1) (Int32.to_int log2_cacheline_bits)   in
            let bitpos = Int32.sub  log2c  (Int32.of_int 1) in
            let byteindex = (Int32.add base_offset  (Int32.div bitpos  (Int32.of_int 8))) in
            let () = Batteries.BitSet.set filt.bits (Int32.to_int (Int32.logor byteindex
                                                                     (Int32.shift_left (Int32.of_int 1)
                                                                        (Int32.to_int (Int32.rem bitpos  (Int32.of_int 8))) ))) in
            if i < num_probes then
              let h = (Int32.add h delta) in
              probe h (i + 1) numprobes base_offset;
        in  probe  h 0 num_probes offset


    let hash_may_match filt h num_lines num_probes  log2_cacheline_bytes =
        let  base_offset = Int32.shift_left (getline h num_lines)  log2_cacheline_bytes in
        hash_maymatch_prepared filt h num_probes  base_offset log2_cacheline_bytes
