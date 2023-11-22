
type 'a s_tree = Leaf | Node of 'a node
and 'a node = { value : 'a; left : 'a s_tree; right : 'a s_tree; }

let node_value n = 
  match n with
  |Leaf ->  0
  | Node {value; _}-> value
let insert=
  Node {
    value = 2;
    left = Node {value = 1; left = Leaf; right = Leaf};
    right = Node {value = 3; left = Leaf; right = Leaf}
  }
(* sTree * insert(key_type i, sTree * t) { *)
(*     /* Insert i into the sTree t, unless it's already there.    */ *)
(*     /* Return a pointer to the resulting sTree.                 */ *)
(*     sTree * new; *)
    
(*     new = (sTree *\) malloc (sizeof (sTree)); *)
(*     if (new == NULL) { *)
(*         printf("Ran out of space\n"); *)
(*         exit(1); *)
(*     } *)
(*     assign_key(new, i); *)
(*     new->value = 1; *)
(*     if (t == NULL) { *)
(*         new->left = new->right = NULL; *)
(*         return new; *)
(*     } *)
(*     t = splay(i,t); *)
(*     if (key_cmp(i, t->key) < 0) { *)
(*         new->left = t->left; *)
(*         new->right = t; *)
(*         t->left = NULL; *)
(*         t->value = 1 + node_value(t->right); *)
(*     } else if (key_cmp(i, t->key) > 0) { *)
(*         new->right = t->right; *)
(*         new->left = t; *)
(*         t->right = NULL; *)
(*         t->value = 1 + node_value(t->left); *)
(*     } else { /* We get here if it's already in the sTree */ *)
(*         /* Don't add it again                      */ *)
(*         free_node(new); *)
(*         assert (t->value == 1 + node_value(t->left) + node_value(t->right)); *)
(*         return t; *)
(*     } *)
(*     new->value = 1 + node_value(new->left) + node_value(new->right); *)
(*     return new; *)
(* } *)
