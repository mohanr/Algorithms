open Containers

module  Layermap = struct
  type t = int64
  let compare v v1 =
    Int64.compare v v1
end


module LayerMap = CCMap.Make(Layermap)

type node =
	 (* stores the left and right child of this node. *)
	value * children * parent
and
  parent = Parent of node
and
  children = Children of  node Array.t
and
  value = Value of int64


(* https://reasonml.chat/t/help-to-create-a-tree-where-nodes-have-reference-to-parents/356/4 *)
 type xfasttrie = {
	layers : node LayerMap.t Array.t;
	root : node;
	number_of_items_in_trie : int64;
	bits : int;
    diff : int;
	min : node;
	max : node
}

let  insert xfasttrie Value  =
  match Value with
  | int -> let bits =
			       Array.get xfasttrie.layers xfasttrie.bits-1 in
           let n =
			       LayerMap.get key bits
	match n with
   | Some n1 ->
  		let new_map = LayerMap.add key n1.value bits in
	       Array.set xfasttrie.layers xfasttrie.bits-1 new_map

	// we need to find a predecessor or successor if it exists
	// to help us set threads later in this method.
	var predecessor, successor *node
	if xfasttrie.min != nil && key < xft.min.entry.Key() {
		successor = xfasttrie.min
	} else {
		successor = xfasttrie.successor(key)
	}

	// only need to find predecessor if successor is nil as otherwise
	// the successor will provide us is the predecessor if it exists.
	if successor == nil {
		if xfasttrie.max != nil && key > xft.max.entry.Key() {
			predecessor = xfasttrie.max
		} else {
			predecessor = xfasttrie.predecessor(key)
		}
	}

	// find the deepest root with a matching prefix, this should
	// save us some time, assuming the hashmap has perfect hashing.
	layer, root := binarySearchHashMaps(xfasttrie.layers, key)
	if root == nil {
		n = xfasttrie.root
		layer = 0
	} else {
		n = root
	}

	var leftOrRight uint64

	// from the existing node, create new nodes.
	for i := uint8(layer); i < xfasttrie.bits; i++ {
		// on 0th, this will be root
		// find out if we need to go left or right
		leftOrRight = (key & positions[xfasttrie.diff+i]) >> (xft.bits - 1 - i)
		if n.children[leftOrRight] == nil || isLeaf(n.children[leftOrRight]) {
			var nn *node
			if i < xfasttrie.bits-1 {
				nn = newNode(n, nil)
			} else {
				nn = newNode(n, entry)
				xfasttrie.num++
			}

			n.children[leftOrRight] = nn
			xfasttrie.layers[i][key&masks[xft.diff+i]] = nn // prefix for this layer
		}

		n = n.children[leftOrRight]
	}

	// we need to put the new node where it belongs in the doubly-linked
	// list comprised of all the leaves.
	if successor != nil { // we have to walk predecessor and successor threads
		predecessor = successor.children[0]
		if predecessor != nil {
			predecessor.children[1] = n
			n.children[0] = predecessor
		}
		n.children[1] = successor
		successor.children[0] = n
	} else if predecessor != nil {
		n.children[0] = predecessor
		predecessor.children[1] = n
	}

	// walk up the successor if it exists to set that branch's new
	// predecessor.
	if successor != nil {
		xfasttrie.walkUpSuccessor(root, n, successor)
	}

	// walk up the predecessor if it exists to set that branch's
	// new successor.
	if predecessor != nil {
		xfasttrie.walkUpPredecessor(root, n, predecessor)
	}

	// finally, walk up our own branch to set both successors
	// and predecessors.
	xfasttrie.walkUpNode(root, n, predecessor, successor)

	// and then do a final check against the min/max indicies.
	if xfasttrie.max == nil || key > xft.max.entry.Key() {
		xfasttrie.max = n
	}

	if xfasttrie.min == nil || key < xft.min.entry.Key() {
		xfasttrie.min = n
	}
}



(* // isInternal returns a bool indicating if the provided *)
(* // node is an internal node, that is, non-leaf node. *)
(* func isInternal(n *node) bool { *)
(* 	if n == nil { *)
(* 		return false *)
(* 	} *)
(* 	return n.entry == nil *)
(* } *)

(* // hasInternal returns a bool indicating if the provided *)
(* // node has a child that is an internal node. *)
(* func hasInternal(n *node) bool { *)
(* 	return isInternal(n.children[0]) || isInternal(n.children[1]) *)
(* } *)

(* // isLeaf returns a bool indicating if the provided node *)
(* // is a leaf node, that is, has a valid entry value. *)
(* func isLeaf(n *node) bool { *)
(* 	if n == nil { *)
(* 		return false *)
(* 	} *)
(* 	return !isInternal(n) *)
(* } *)

(* // Entry defines items that can be inserted into the x-fast *)
(* // trie. *)
(* type Entry interface { *)
(* 	// Key is the key for this entry.  If the trie has been *)
(* 	// given bit size n, only the last n bits of this key *)
(* 	// will matter.  Use a bit size of 64 to enable all *)
(* 	// 2^64-1 keys. *)
(* 	Key() uint64 *)
(* } *)

(* // masks are used to determine the prefix of any given key.  The masks *)
(* // are stored in a [64] array where each position of the array represents *)
(* // a bitmask to the ith bit.  For example, if you wanted to mask the first *)
(* // bit of a 64-bit int you'd and it with masks[0].  If you wanted to mask *)
(* // the first bit of an 8 bit key, you'd have to shift 56 bits to the right *)
(* // and perform the mask operation.  This array is immutable and should not *)
(* // be changed after initialization. *)
(* var masks = func() [64]uint64 { // we don't technically need the last mask, this is just to be consistent *)
(* 	masks := [64]uint64{} *)
(* 	mask := uint64(0) *)
(* 	for i := uint64(0); i < 64; i++ { *)
(* 		mask = mask | 1<<(63-i) *)
(* 		masks[i] = mask *)
(* 	} *)
(* 	return masks *)
(* }() *)

(* // positions are similar to masks and that the positions array allows *)
(* // us to determine if a node should go left or right at a specific bit *)
(* // position of the key.  Basically, this array stores every 2^n number *)
(* // where n is in [0, 64).  This array is immutable and should not be *)
(* // changed after initialization. *)
(* var positions = func() [64]uint64 { *)
(* 	positions := [64]uint64{} *)
(* 	for i := uint64(0); i < 64; i++ { *)
(* 		positions[i] = uint64(1 << (63 - i)) *)
(* 	} *)
(* 	return positions *)
(* }() *)

(* type node struct { *)
(* 	// entry will store the entry for this node.  Is nil for *)
(* 	// every internal node and non-nil for all leaves.  It is *)
(* 	// how the internal/leaf function helpers determine the *)
(* 	// position of this node. *)
(* 	entry Entry *)
(* 	// children stores the left and right child of this node. *)
(* 	// At any time, and at any layer, it's possible for a pointer *)
(* 	// to a child to point to a leaf due to threading. *)
(* 	children [2]*node *)
(* 	// i hate this, but it is really the best way *)
(* 	// to walk up successor and predecessor threads *)
(* 	parent *node *)
(* } *)

(* // newNode will allocate and initialize a newNode with the provided *)
(* // parent and entry.  Parent should never be nil, but entry may be *)
(* // if constructing an internal node. *)
(* func newNode(parent *node, entry Entry) *node { *)
(* 	return &node{ *)
(* 		children: [2]*node{}, *)
(* 		entry:    entry, *)
(* 		parent:   parent, *)
(* 	} *)
(* } *)

(* // binarySearchHashMaps will perform a binary search of the provided *)
(* // maps to return a node that matches the longest prefix of the provided *)
(* // key.  This will return nil if a match could not be found, which would *)
(* // also return layer 0.  Layer information is useful when determining the *)
(* // distance from the provided node to the leaves. *)
(* func binarySearchHashMaps(layers []map[uint64]*node, key uint64) (int, *node) { *)
(* 	low, high := 0, len(layers)-1 *)
(* 	diff := 64 - len(layers) *)
(* 	var mid int *)
(* 	var node *node *)
(* 	for low <= high { *)
(* 		mid = (low + high) / 2 *)
(* 		n, ok := layers[mid][key&masks[diff+mid]] *)
(* 		if ok { *)
(* 			node = n *)
(* 			low = mid + 1 *)
(* 		} else { *)
(* 			high = mid - 1 *)
(* 		} *)
(* 	} *)

(* 	return low, node *)
(* } *)

(* // whichSide returns an int representing the side on which *)
(* // the node resides in its parent.  NOTE: this function will panic *)
(* // if the child does not within the parent.  This situation should *)
(* // should be caught as early as possible as if it happens data *)
(* // coming from the x-fast trie cannot be trusted. *)
(* func whichSide(n, parent *node) int { *)
(* 	if parent.children[0] == n { *)
(* 		return 0 *)
(* 	} *)

(* 	if parent.children[1] == n { *)
(* 		return 1 *)
(* 	} *)

(* 	panic(fmt.Sprintf(`Node: %+v, %p not a child of: %+v, %p`, n, n, parent, parent)) *)
(* } *)


(* type XFastTrie struct { *)
(* 	// layers stores the hashmaps of the individual layers of the trie. *)
(* 	// The hashmaps store prefixes, allowing use to do a binary search *)
(* 	// of these maps before visiting the trie for successor/predecessor *)
(* 	// queries. *)
(* 	layers []map[uint64]*node *)
(* 	// root is a pointer to the first node of the trie, which actually *)
(* 	// adds an additional layer, ie, instead of 64 layers for a *)
(* 	// uint64, this will cause the number of layers to be 65. *)
(* 	root *node *)
(* 	// num is the number of items in the trie. *)
(* 	num uint64 *)
(* 	// bits represents the number of bits of the keys this trie *)
(* 	// expects.  Because the time complexity of operations is *)
(* 	// dependent upon universe size, smaller sized keys will *)
(* 	// actually cause the trie to be faster.  Diff is the difference *)
(* 	// between the desired bit size and 64 as we have to offset *)
(* 	// in the position and mask arrays. *)
(* 	bits, diff uint8 *)
(* 	// min and max index the lowest and highest seen keys respectively. *)
(* 	// this immediately allows us to check a desired key against *)
(* 	// constraints and allows min/max operations to be performed *)
(* 	// in O(1) time. *)
(* 	min, max *node *)
(* } *)

(* // init will initialize the XFastTrie with the provided byte-size. *)
(* // I'd prefer generics here, but it is what it is.  We expect uints *)
(* // here when ints would perform just as well, but the public methods *)
(* // on the XFastTrie all expect uint64, so we expect a uint in the *)
(* // constructor for consistency's sake. *)
(* func (xft *XFastTrie) init(intType interface{}) { *)
(* 	bits := uint8(0) *)
(* 	switch intType.(type) { *)
(* 	case uint8: *)
(* 		bits = 8 *)
(* 	case uint16: *)
(* 		bits = 16 *)
(* 	case uint32: *)
(* 		bits = 32 *)
(* 	case uint, uint64: *)
(* 		bits = 64 *)
(* 	default: *)
(* 		// we'll panic with a bad value to the constructor. *)
(* 		panic(`Invalid universe size provided.`) *)
(* 	} *)

(* 	xft.layers = make([]map[uint64]*node, bits) *)
(* 	xft.bits = bits *)
(* 	xft.diff = 64 - bits *)
(* 	for i := uint8(0); i < bits; i++ { *)
(* 		xft.layers[i] = make(map[uint64]*node, 50) // we can obviously be more intelligent about this. *)
(* 	} *)
(* 	xft.num = 0 *)
(* 	xft.root = newNode(nil, nil) *)
(* } *)

(* // Exists returns a bool indicating if the provided *)
(* // key exists in the trie.  This is an O(1) operation. *)
(* func (xft *XFastTrie) Exists(key uint64) bool { *)
(* 	// the bottom hashmap of the trie has every entry *)
(* 	// in it. *)
(* 	_, ok := xft.layers[xft.bits-1][key] *)
(* 	return ok *)
(* } *)

(* // Len returns the number of items in this trie.  This is an *)
(* // O(1) operation. *)
(* func (xft *XFastTrie) Len() uint64 { *)
(* 	return xft.num *)
(* } *)

(* // Max will return the highest keyed value in the trie.  This is *)
(* // an O(1) operation. *)
(* func (xft *XFastTrie) Max() Entry { *)
(* 	if xft.max == nil { *)
(* 		return nil *)
(* 	} *)

(* 	return xft.max.entry *)
(* } *)

(* // Min will return the lowest keyed value in the trie.  This is *)
(* // an O(1) operation. *)
(* func (xft *XFastTrie) Min() Entry { *)
(* 	if xft.min == nil { *)
(* 		return nil *)
(* 	} *)

(* 	return xft.min.entry *)
(* } *)

(* // insert will add the provided entry to the trie or overwrite the existing *)
(* // entry if it exists. *)
(* func (xft *XFastTrie) insert(entry Entry) { *)
(* 	key := entry.Key() // so we aren't calling this interface method over and over, fucking Go *)
(* 	n := xft.layers[xft.bits-1][key] *)
(* 	if n != nil { *)
(* 		n.entry = entry *)
(* 		return *)
(* 	} *)

(* 	// we need to find a predecessor or successor if it exists *)
(* 	// to help us set threads later in this method. *)
(* 	var predecessor, successor *node *)
(* 	if xft.min != nil && key < xft.min.entry.Key() { *)
(* 		successor = xft.min *)
(* 	} else { *)
(* 		successor = xft.successor(key) *)
(* 	} *)

(* 	// only need to find predecessor if successor is nil as otherwise *)
(* 	// the successor will provide us is the predecessor if it exists. *)
(* 	if successor == nil { *)
(* 		if xft.max != nil && key > xft.max.entry.Key() { *)
(* 			predecessor = xft.max *)
(* 		} else { *)
(* 			predecessor = xft.predecessor(key) *)
(* 		} *)
(* 	} *)

(* 	// find the deepest root with a matching prefix, this should *)
(* 	// save us some time, assuming the hashmap has perfect hashing. *)
(* 	layer, root := binarySearchHashMaps(xft.layers, key) *)
(* 	if root == nil { *)
(* 		n = xft.root *)
(* 		layer = 0 *)
(* 	} else { *)
(* 		n = root *)
(* 	} *)

(* 	var leftOrRight uint64 *)

(* 	// from the existing node, create new nodes. *)
(* 	for i := uint8(layer); i < xft.bits; i++ { *)
(* 		// on 0th, this will be root *)
(* 		// find out if we need to go left or right *)
(* 		leftOrRight = (key & positions[xft.diff+i]) >> (xft.bits - 1 - i) *)
(* 		if n.children[leftOrRight] == nil || isLeaf(n.children[leftOrRight]) { *)
(* 			var nn *node *)
(* 			if i < xft.bits-1 { *)
(* 				nn = newNode(n, nil) *)
(* 			} else { *)
(* 				nn = newNode(n, entry) *)
(* 				xft.num++ *)
(* 			} *)

(* 			n.children[leftOrRight] = nn *)
(* 			xft.layers[i][key&masks[xft.diff+i]] = nn // prefix for this layer *)
(* 		} *)

(* 		n = n.children[leftOrRight] *)
(* 	} *)

(* 	// we need to put the new node where it belongs in the doubly-linked *)
(* 	// list comprised of all the leaves. *)
(* 	if successor != nil { // we have to walk predecessor and successor threads *)
(* 		predecessor = successor.children[0] *)
(* 		if predecessor != nil { *)
(* 			predecessor.children[1] = n *)
(* 			n.children[0] = predecessor *)
(* 		} *)
(* 		n.children[1] = successor *)
(* 		successor.children[0] = n *)
(* 	} else if predecessor != nil { *)
(* 		n.children[0] = predecessor *)
(* 		predecessor.children[1] = n *)
(* 	} *)

(* 	// walk up the successor if it exists to set that branch's new *)
(* 	// predecessor. *)
(* 	if successor != nil { *)
(* 		xft.walkUpSuccessor(root, n, successor) *)
(* 	} *)

(* 	// walk up the predecessor if it exists to set that branch's *)
(* 	// new successor. *)
(* 	if predecessor != nil { *)
(* 		xft.walkUpPredecessor(root, n, predecessor) *)
(* 	} *)

(* 	// finally, walk up our own branch to set both successors *)
(* 	// and predecessors. *)
(* 	xft.walkUpNode(root, n, predecessor, successor) *)

(* 	// and then do a final check against the min/max indicies. *)
(* 	if xft.max == nil || key > xft.max.entry.Key() { *)
(* 		xft.max = n *)
(* 	} *)

(* 	if xft.min == nil || key < xft.min.entry.Key() { *)
(* 		xft.min = n *)
(* 	} *)
(* } *)

(* // walkUpSuccessor will walk up the successor branch setting *)
(* // the predecessor where possible.  This breaks when a common *)
(* // ancestor between successor and node is found, ie, the root. *)
(* func (xft *XFastTrie) walkUpSuccessor(root, node, successor *node) { *)
(* 	n := successor.parent *)
(* 	for n != nil && n != root { *)
(* 		// we don't really want to overwrite existing internal nodes, *)
(* 		// or where the child is a leaf that is the successor *)
(* 		if !isInternal(n.children[0]) && n.children[0] != successor { *)
(* 			n.children[0] = node *)
(* 		} *)
(* 		n = n.parent *)
(* 	} *)
(* } *)

(* // walkUpPredecessor will walk up the predecessor branch setting *)
(* // the successor where possible.  This breaks when a common *)
(* // ancestor between predecessor and node is found, ie, the root. *)
(* func (xft *XFastTrie) walkUpPredecessor(root, node, predecessor *node) { *)
(* 	n := predecessor.parent *)
(* 	for n != nil && n != root { *)
(* 		if !isInternal(n.children[1]) && n.children[1] != predecessor { *)
(* 			n.children[1] = node *)
(* 		} *)
(* 		n = n.parent *)
(* 	} *)
(* } *)

(* // walkUpNode will walk up the newly created branch and set predecessor *)
(* // and successor where possible.  If predecessor or successor are nil, *)
(* // this will set nil where possible. *)
(* func (xft *XFastTrie) walkUpNode(root, node, predecessor, successor *node) { *)
(* 	n := node.parent *)
(* 	for n != nil && n != root { *)
(* 		if !isInternal(n.children[1]) && n.children[1] != successor && n.children[1] != node { *)
(* 			n.children[1] = successor *)
(* 		} *)
(* 		if !isInternal(n.children[0]) && n.children[0] != predecessor && n.children[0] != node { *)
(* 			n.children[0] = predecessor *)
(* 		} *)
(* 		n = n.parent *)
(* 	} *)
(* } *)

(* // Insert will insert the provided entries into the trie.  Any entry *)
(* // with an existing key will cause an overwrite.  This is an O(log M) *)
(* // operation, for each entry. *)
(* func (xft *XFastTrie) Insert(entries ...Entry) { *)
(* 	for _, e := range entries { *)
(* 		xft.insert(e) *)
(* 	} *)
(* } *)

(* func (xft *XFastTrie) delete(key uint64) { *)
(* 	n := xft.layers[xft.bits-1][key] *)
(* 	if n == nil { // there's no matching k, v pair *)
(* 		return *)
(* 	} *)

(* 	successor, predecessor := n.children[1], n.children[0] *)

(* 	i := uint8(1) *)
(* 	delete(xft.layers[xft.bits-1], key) *)
(* 	leftOrRight := whichSide(n, n.parent) *)
(* 	n.parent.children[leftOrRight] = nil *)
(* 	n.children[0], n.children[1] = nil, nil *)
(* 	n = n.parent *)
(* 	hasImmediateSibling := false *)
(* 	if successor != nil && successor.parent == n { *)
(* 		hasImmediateSibling = true *)
(* 	} *)
(* 	if predecessor != nil && predecessor.parent == n { *)
(* 		hasImmediateSibling = true *)
(* 	} *)

(* 	// this loop will kill any nodes that no longer link to internal *)
(* 	// nodes *)
(* 	for n != nil && n.parent != nil { *)
(* 		// if we have an internal node remaining we should abort *)
(* 		// now as no further node will be removed.  We should also *)
(* 		// abort if the first parent of a leaf references the pre *)
(* 		if hasInternal(n) || (i == 1 && hasImmediateSibling) { *)
(* 			n = n.parent // we had one side deleted, need to set the other *)
(* 			break *)
(* 		} *)

(* 		leftOrRight = whichSide(n, n.parent) *)
(* 		n.parent.children[leftOrRight] = nil *)
(* 		n.children[0], n.children[1] = nil, nil *)
(* 		delete(xft.layers[xft.bits-i-1], key&masks[len(masks)-1-int(i)]) *)
(* 		n = n.parent *)
(* 		i++ *)
(* 	} *)

(* 	// we need to check now and update threads, but in the leaves *)
(* 	// and in their branches *)
(* 	if predecessor != nil { *)
(* 		predecessor.children[1] = successor *)
(* 		xft.walkUpPredecessor(n, successor, predecessor) *)
(* 	} *)

(* 	if successor != nil { *)
(* 		successor.children[0] = predecessor *)
(* 		xft.walkUpSuccessor(n, predecessor, successor) *)
(* 	} *)

(* 	// check max/min indices *)
(* 	if xft.max.entry.Key() == key { *)
(* 		xft.max = predecessor *)
(* 	} *)

(* 	if xft.min.entry.Key() == key { *)
(* 		xft.min = successor *)
(* 	} *)

(* 	// decrement number of nodes *)
(* 	xft.num-- *)
(* } *)

(* // Delete will delete the provided keys from the trie.  If an entry *)
(* // associated with a provided key cannot be found, that deletion is *)
(* // a no-op.  Each deletion is an O(log M) operation. *)
(* func (xft *XFastTrie) Delete(keys ...uint64) { *)
(* 	for _, key := range keys { *)
(* 		xft.delete(key) *)
(* 	} *)
(* } *)

(* // predecessor will find the node equal to or immediately less *)
(* // than the provided key. *)
(* func (xft *XFastTrie) predecessor(key uint64) *node { *)
(* 	if xft.root == nil || xft.max == nil { // no successor if no nodes *)
(* 		return nil *)
(* 	} *)

(* 	if key >= xft.max.entry.Key() { *)
(* 		return xft.max *)
(* 	} *)

(* 	if key < xft.min.entry.Key() { *)
(* 		return nil *)
(* 	} *)

(* 	n := xft.layers[xft.bits-1][key] *)
(* 	if n != nil { *)
(* 		return n *)
(* 	} *)

(* 	layer, n := binarySearchHashMaps(xft.layers, key) *)
(* 	if n == nil && layer > 1 { *)
(* 		return nil *)
(* 	} else if n == nil { *)
(* 		n = xft.root *)
(* 	} *)

(* 	if isInternal(n.children[0]) && isLeaf(n.children[1]) { *)
(* 		return n.children[1].children[0] *)
(* 	} *)
(* 	return n.children[0] *)
(* } *)

(* // successor will find the node equal to or immediately more *)
(* // than the provided key. *)
(* func (xft *XFastTrie) successor(key uint64) *node { *)
(* 	if xft.root == nil || xft.min == nil { // no successor if no nodes *)
(* 		return nil *)
(* 	} *)

(* 	if key <= xft.min.entry.Key() { *)
(* 		return xft.min *)
(* 	} *)

(* 	if key > xft.max.entry.Key() { *)
(* 		return nil *)
(* 	} *)

(* 	n := xft.layers[xft.bits-1][key] *)
(* 	if n != nil { *)
(* 		return n *)
(* 	} *)

(* 	layer, n := binarySearchHashMaps(xft.layers, key) *)
(* 	if n == nil && layer > 1 { *)
(* 		return nil *)
(* 	} else if n == nil { *)
(* 		n = xft.root *)
(* 	} *)

(* 	if isInternal(n.children[1]) && isLeaf(n.children[0]) { *)
(* 		return n.children[0].children[1] *)
(* 	} *)
(* 	return n.children[1] *)
(* } *)

(* // Successor will return an Entry which matches the provided *)
(* // key or its immediate successor.  Will return nil if a successor *)
(* // does not exist.  This is an O(log log M) operation. *)
(* func (xft *XFastTrie) Successor(key uint64) Entry { *)
(* 	n := xft.successor(key) *)
(* 	if n == nil { *)
(* 		return nil *)
(* 	} *)

(* 	return n.entry *)
(* } *)

(* // Predecessor will return an Entry which matches the provided *)
(* // key or its immediate predecessor.  Will return nil if a predecessor *)
(* // does not exist.  This is an O(log log M) operation. *)
(* func (xft *XFastTrie) Predecessor(key uint64) Entry { *)
(* 	n := xft.predecessor(key) *)
(* 	if n == nil { *)
(* 		return nil *)
(* 	} *)

(* 	return n.entry *)
(* } *)

(* // Iter will return an iterator that will iterate over all values *)
(* // equal to or immediately greater than the provided key.  Iterator *)
(* // will iterate successor relationships. *)
(* func (xft *XFastTrie) Iter(key uint64) *Iterator { *)
(* 	return &Iterator{ *)
(* 		n:     xft.successor(key), *)
(* 		first: true, *)
(* 	} *)
(* } *)

(* // Get will return a value in the trie associated with the provided *)
(* // key if it exists.  Returns nil if the key does not exist.  This *)
(* // is expected to take O(1) time. *)
(* func (xft *XFastTrie) Get(key uint64) Entry { *)
(* 	// only have to check the last hashmap for the provided *)
(* 	// key. *)
(* 	n := xft.layers[xft.bits-1][key] *)
(* 	if n == nil { *)
(* 		return nil *)
(* 	} *)

(* 	return n.entry *)
(* } *)

(* // New will construct a new X-Fast Trie with the given "size," *)
(* // that is the size of the universe of the trie.  This expects *)
(* // a uint of some sort, ie, uint8, uint16, etc.  The size of the *)
(* // universe will be 2^n-1 and will affect the speed of all operations. *)
(* // IFC MUST be a uint type. *)
(* func New(ifc interface{}) *XFastTrie { *)
(* 	xft := &XFastTrie{} *)
(* 	xft.init(ifc) *)
(* 	return xft *)
(* } *)
