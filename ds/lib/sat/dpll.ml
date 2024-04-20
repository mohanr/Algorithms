let clauses  c : int list=
    let rec accu acc i : int list=
                   if ( i >= 0 ) then
                       accu (int_of_char (c.[i]) :: acc) (i - 1)
                   else
                       acc
    in
    accu []  (String.length c - 1)
