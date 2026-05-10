open Core
open Tinyest

module StringIntTuple = struct
  module T = struct
   type t = string * expr
   [@@deriving_show]
   let compare (x0, y0) (x1, y1) =
     match String.compare x0 x1 with
         0 -> compare_expr y0 y1
       | c -> c
   let t_of_sexp tuple = Tuple2.t_of_sexp String.t_of_sexp expr_of_sexp tuple

   let sexp_of_t tuple = Tuple2.sexp_of_t String.sexp_of_t sexp_of_expr tuple
  end
  include T
  include Comparable.Make(T)
end


module StringIntTupleSet = Set.Make(StringIntTuple )
module StringIntTupleSetofSet = Set.Make(StringIntTupleSet )
