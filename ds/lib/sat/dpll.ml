let clauses  c : int list=
    let rec accu acc i : int list=
                   if ( i >= 0 ) then
                       accu (int_of_char (c.[i]) :: acc) (i - 1)
                   else
                       acc
    in
    accu []  (String.length c - 1)

(* let backtracking formula assignment = *)

(* def pure_literal(formula): *)
(*     counter = get_counter(formula) *)
(*     assignment = [] *)
(*     pures = []  # [ x for x,y in counter.items() if -x not in counter ] *)
(*     for literal, times in counter.items(): *)
(*         if -literal not in counter: pures.append(literal) *)
(*     for pure in pures: *)
(*         formula = bcp(formula, pure) *)
(*     assignment += pures *)
(*     return formula, assignment *)

(* def bcp(formula, unit): *)
(*     modified = [] *)
(*     for clause in formula: *)
(*         if unit in clause: continue *)
(*         if -unit in clause: *)
(*             c = [x for x in clause if x != -unit] *)
(*             if len(c) == 0: return -1 *)
(*             modified.append(c) *)
(*         else: *)
(*             modified.append(clause) *)
(*     return modified *)


(* def get_counter(formula): *)
(*     counter = {} *)
(*     for clause in formula: *)
(*         for literal in clause: *)
(*             if literal in counter: *)
(*                 counter[literal] += 1 *)
(*             else: *)
(*                 counter[literal] = 1 *)
(*     return counter *)
