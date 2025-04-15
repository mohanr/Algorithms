open Bloomfilter__.Btree_variant

let%expect_test _=

  let tree  = LF in
  let tree = insert 1 tree in
  let tree = insert 2 tree in
  let tree = insert 3 tree in
  let tree = insert 4 tree in
  let tree = insert 5 tree in
  let tree = insert 6 tree in

  print_bTree tree 0;
  [%expect {|
    Type1Type2Type1Type1Type1Type1Type1Type2Type2Type1Type1Type1            1
              3 5
                2 6
                4 |}]

