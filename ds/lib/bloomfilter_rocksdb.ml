open Batteries

module type BLOOM_MATH = sig

  val standard_fprate :  float -> float -> float
  val finger_print_fprate : float -> float -> float
  val cache_local_fprate : float -> float -> float -> float
  val independent_probability_sum  :  float -> float -> float

end

module  Bloom : BLOOM_MATH = struct

  let standard_fprate bits_per_key num_probes : float =
     Float.pow (1. -. Float.exp (-. num_probes /. bits_per_key)) num_probes

  let cache_local_fprate bits_per_key num_probes
                                 cache_line_bits =
    if bits_per_key <= 0.0 then
      1.0
    else

    let keys_per_cache_line = cache_line_bits /. bits_per_key in
    let keys_stddev = sqrt keys_per_cache_line in
    let crowded_fp = standard_fprate (
        cache_line_bits /. (keys_per_cache_line +. keys_stddev)) num_probes in
    let uncrowded_fp = standard_fprate (
        cache_line_bits /. (keys_per_cache_line -. keys_stddev)) num_probes in
    (crowded_fp +. uncrowded_fp) /. 2.

  let finger_print_fprate num_keys fingerprint_bits : float =
    let inv_fingerprint_space = Float.pow 0.5 fingerprint_bits in
    let base_estimate = num_keys *. inv_fingerprint_space in
    if base_estimate > 0.0001 then
      1.0 -. Float.exp (-.base_estimate)
    else
      base_estimate -. (base_estimate *. base_estimate *. 0.5)

  let independent_probability_sum rate1 rate2 =
    rate1 +. rate2 -. (rate1 *. rate2)

end

   open Bloom
   type 'bloombits filter =
   {
     bits : Batteries.BitSet.t
   }

   let estimated_fprate keys bytes num_probes =
        let bits_per_key = 8.0 *. bytes /. keys in
        let filterRate = cache_local_fprate bits_per_key num_probes 512. in (* Cache line size is 512 *)
        let filter_rate  = filterRate +. 0.1 /. (bits_per_key *. 0.75 +. 22.) in
        let finger_print_rate = finger_print_fprate keys 32. in
        independent_probability_sum filter_rate finger_print_rate

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
