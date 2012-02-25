note
	model: item, left_child, right_child, parent, child_index, object_comparison

class
	FLAT_BINARY_TREE [G]

inherit
	ANY
		redefine
			copy,
			is_equal
		end

create
	make

feature -- Initialization

	make (v: like item)
			-- Create a root node with value `v'.
		do
			item := v
		ensure
			item_effect: item = v
			left_child_effect: left_child = Void
			right_child_effect: right_child = Void
			parent_effect: parent = Void
			child_index_effect: child_index = 0
			object_comparison_effect: not object_comparison
		end

feature -- Access

	child: like parent
			-- Child at cursor position
		require
			readable: readable_child
		do
			inspect child_index
			when 1 then
				Result := left_child
			when 2 then
				Result := right_child
			else
				Result := Void
			end
		ensure
			definition_first: child_index = 1 implies Result = left_child
			definition_second: child_index /= 1 implies Result = right_child
		end

	child_cursor: FLAT_ARRAYED_LIST_CURSOR
			-- Current cursor position
		do
			create Result.make (child_index)
		end

	child_index: INTEGER_32
			-- Index of cursor position

	child_item: like item
			-- Item in current child node
			-- (from TREE)
		require -- from TREE
			readable: child_readable
		local
			c: like child
		do
			c := child
			check
				c_attached: c /= Void
			end
			Result := c.item
		end

	first_child: like parent
			-- Left child
		require -- from TREE
			is_not_leaf: not is_leaf
		do
			Result := left_child
		end

	item: G
			-- Content of cell.
			-- (from CELL)

	last_child: like parent
			-- Right child
		require -- from TREE
			is_not_leaf: not is_leaf
		do
			Result := right_child
		end

	left_child: like parent
			-- Left child, if any

	left_item: like item
			-- Value of left child
		require
			has_left: left_child /= Void
		local
			l: like left_child
		do
			l := left_child
			check
				l_attached: l /= Void
			end
			Result := l.item
		end

	left_sibling: like parent
			-- Left neighbor, if any
		require -- from TREE
			is_not_root: not is_root
		local
			p: like parent
		do
			p := parent
			if p /= Void and then p.right_child = Current then
				Result := p.left_child
			end
		ensure -- from TREE
			is_sibling: Result /= Void implies is_sibling (Result)
			right_is_current: (Result /= Void) implies (Result.right_sibling = Current)
		end

	parent: like Current
			-- Parent of current node

	right_child: like parent
			-- Right child, if any

	right_item: like item
			-- Value of right child
		require
			has_right: right_child /= Void
		local
			r: like right_child
		do
			r := right_child
			check
				r_attached: r /= Void
			end
			Result := r.item
		end

	right_sibling: like parent
			-- Right neighbor, if any
		require -- from TREE
			is_not_root: not is_root
		local
			p: like parent
		do
			p := parent
			if p /= Void and then p.left_child = Current then
				Result := p.right_child
			end
		ensure -- from TREE
			is_sibling: Result /= Void implies is_sibling (Result)
			left_is_current: (Result /= Void) implies (Result.left_sibling = Current)
		end

feature -- Measurement

	arity: INTEGER_32
			-- Number of children
		require -- from  TREE
			True
		do
			if has_left then
				Result := Result + 1
			end
			if has_right then
				Result := Result + 1
			end
		ensure then
			valid_arity: Result <= Child_capacity
		end

	Child_capacity: INTEGER_32 = 2
			-- Maximum number of children

	count: INTEGER_32
			-- Number of items
			-- (from TREE)
		do
			Result := subtree_count + 1
		end

feature -- Comparison

	is_equal (other: FLAT_BINARY_TREE [G]): BOOLEAN
			-- Does `other' contain the same elements?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from TREE)
		do
			if Current = other then
				Result := True
			else
				Result := (is_empty = other.is_empty) and (object_comparison = other.object_comparison) and (Child_capacity = other.Child_capacity)
				if Result and not is_empty then
					Result := tree_is_equal (Current, other)
				end
			end
		end

	node_is_equal (other: FLAT_BINARY_TREE [G]): BOOLEAN
			-- Is `other' equal to Current?
			-- (from TREE)
		require -- from TREE
			other_not_void: other /= Void
		do
			if object_comparison then
				Result := item ~ other.item
			else
				Result := item = other.item
			end
		end

feature -- Status report

	changeable_comparison_criterion: BOOLEAN
			-- May object_comparison be changed?
			-- (Answer: yes by default.)
			-- (from CONTAINER)
		do
			Result := True
		end

	child_after: BOOLEAN
			-- Is there no valid child position to the right of cursor?
		do
			Result := child_index >= Child_capacity + 1
		end

	child_before: BOOLEAN
			-- Is there no valid child position to the left of cursor?
			-- (from TREE)
		do
			Result := child_index = 0
		end

	child_isfirst: BOOLEAN
			-- Is cursor under first child?
			-- (from TREE)
		do
			Result := not is_leaf and child_index = 1
		ensure -- from TREE
			not_is_leaf: Result implies not is_leaf
		end

	child_islast: BOOLEAN
			-- Is cursor under last child?
			-- (from TREE)
		do
			Result := not is_leaf and child_index = Child_capacity
		ensure -- from TREE
			not_is_leaf: Result implies not is_leaf
		end

	child_off: BOOLEAN
			-- Is there no current child?
			-- (from TREE)
		do
			Result := child_before or child_after
		end

	child_readable: BOOLEAN
			-- Is there a current child_item to be read?
			-- (from TREE)
		do
			Result := not child_off and then (child /= Void)
		end

	child_writable: BOOLEAN
			-- Is there a current child_item that may be modified?
			-- (from TREE)
		do
			Result := not child_off and then (child /= Void)
		end

	has (v: G): BOOLEAN
			-- Does subtree include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from TREE)
		do
			if object_comparison then
				Result := v ~ item or else subtree_has (v)
			else
				Result := v = item or else subtree_has (v)
			end
		ensure -- from CONTAINER
			not_found_in_empty: Result implies not is_empty
		end

	has_both: BOOLEAN
			-- Has current node two children?
		do
			Result := left_child /= Void and right_child /= Void
		ensure
			Result = (has_left and has_right)
		end

	has_left: BOOLEAN
			-- Has current node a left child?
		do
			Result := left_child /= Void
		ensure
			Result = (left_child /= Void)
		end

	has_none: BOOLEAN
			-- Are there no children?
			-- Was declared in FLAT_BINARY_TREE as synonym of is_leaf.
		do
			Result := left_child = Void and right_child = Void
		end

	has_right: BOOLEAN
			-- Has current node a right child?
		do
			Result := right_child /= Void
		ensure
			Result = (right_child /= Void)
		end

	is_empty: BOOLEAN
			-- Is structure empty of items?
			-- (from TREE)
		do
			Result := False
		end

	is_leaf: BOOLEAN
			-- Are there no children?
			-- Was declared in FLAT_BINARY_TREE as synonym of has_none.
		do
			Result := left_child = Void and right_child = Void
		end

	is_root: BOOLEAN
			-- Is there no parent?
			-- (from TREE)
		do
			Result := parent = Void
		end

	is_sibling (other: like parent): BOOLEAN
			-- Are current node and `other' siblings?
			-- (from TREE)
		require -- from TREE
			other_exists: other /= Void
		do
			Result := not is_root and other.parent = parent
		ensure -- from TREE
			not_root: Result implies not is_root
			other_not_root: Result implies not other.is_root
			same_parent: Result = not is_root and other.parent = parent
		end

	object_comparison: BOOLEAN
			-- Must search operations use equal rather than `='
			-- for comparing references? (Default: no, use `='.)
			-- (from CONTAINER)

	Readable: BOOLEAN = True
			-- (from TREE)

	readable_child: BOOLEAN
			-- Is there a current child to be read?
			-- (from TREE)
		do
			Result := not child_off
		end

	valid_cursor_index (i: INTEGER_32): BOOLEAN
			-- Is `i' correctly bounded for cursor movement?
			-- (from TREE)
		do
			Result := (i >= 0) and (i <= Child_capacity + 1)
		ensure -- from TREE
			valid_cursor_index_definition: Result = (i >= 0) and (i <= Child_capacity + 1)
		end

	Writable: BOOLEAN = True
			-- Is there a current item that may be modified?
			-- (from TREE)

	writable_child: BOOLEAN
			-- Is there a current child that may be modified?
			-- (from TREE)
		do
			Result := not child_off
		end

feature -- Status setting

	compare_objects
			-- Ensure that future search operations will use equal
			-- rather than `=' for comparing references.
			-- (from CONTAINER)
		require -- from CONTAINER
			changeable_comparison_criterion: changeable_comparison_criterion
		do
			object_comparison := True
		ensure -- from CONTAINER
			object_comparison
		end

	compare_references
			-- Ensure that future search operations will use `='
			-- rather than equal for comparing references.
			-- (from CONTAINER)
		require -- from CONTAINER
			changeable_comparison_criterion: changeable_comparison_criterion
		do
			object_comparison := False
		ensure -- from CONTAINER
			reference_comparison: not object_comparison
		end

feature -- Cursor movement

	child_back
			-- Move cursor to previous child.
		do
			child_index := child_index - 1
		end

	child_finish
			-- Move cursor to last child.
		do
			child_index := arity
		end

	child_forth
			-- Move cursor to next child.
		do
			child_index := child_index + 1
		end

	child_go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th child.
		do
			child_index := i
		ensure then -- from TREE
			position: child_index = i
		end

	child_go_to (p: FLAT_ARRAYED_LIST_CURSOR)
			-- Move cursor to child remembered by `p'.
		do
			child_index := p.index
		end

	child_start
			-- Move to first child.
		do
			if has_left then
				child_index := 1
			elseif has_right then
				child_index := 2
			else
				child_index := 0
			end
		end

feature -- Element change

	child_put (v: like item)
			-- Put `v' at current child position.
			-- Was declared in FLAT_BINARY_TREE as synonym of child_replace.
		require -- from TREE
			child_writable: child_writable
		local
			node: like Current
			c: like child
		do
			c := child
			if c /= Void then
				if object_comparison then
					c.compare_objects
				else
					c.compare_references
				end
				c.put (v)
			else
				create node.make (v)
				if object_comparison then
					node.compare_objects
				end
				put_child (node)
			end
		ensure -- from TREE
			item_inserted: child_item = v
		end

	child_replace (v: like item)
			-- Put `v' at current child position.
			-- Was declared in FLAT_BINARY_TREE as synonym of child_put.
		require -- from TREE
			child_writable: child_writable
		local
			node: like Current
			c: like child
		do
			c := child
			if c /= Void then
				if object_comparison then
					c.compare_objects
				else
					c.compare_references
				end
				c.put (v)
			else
				create node.make (v)
				if object_comparison then
					node.compare_objects
				end
				put_child (node)
			end
		ensure -- from TREE
			item_inserted: child_item = v
		end

	fill (other: FLAT_BINARY_TREE [G])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current node
			-- need not be the same.
			-- (from TREE)
		do
			replace (other.item)
			fill_subtree (other)
		end

	put (v: like item)
			-- Make `v' the cell's item.
			-- Was declared in CELL as synonym of replace.
			-- (from CELL)
		require -- from TREE
			is_writable: Writable
		do
			item := v
		ensure -- from TREE
			item_inserted: item = v
		end

	put_child (n: like new_tree)
			-- Put `n' at current child position.
			-- Was declared in FLAT_BINARY_TREE as synonym of replace_child.
		require -- from TREE
			non_void_argument: n /= Void
		do
			if object_comparison then
				n.compare_objects
			else
				n.compare_references
			end
			n.attach_to_parent (Void)
			if not has_left and not has_right then
				child_index := 1
			end
			inspect child_index
			when 1 then
				put_left_child (n)
			when 2 then
				put_right_child (n)
			end
		end

	put_left_child (n: like parent)
			-- Set left_child to `n'.
		require
			no_parent: n = Void or else n.is_root
		local
			l: like left_child
		do
			if n /= Void then
				if object_comparison then
					n.compare_objects
				else
					n.compare_references
				end
			end
			l := left_child
			if l /= Void then
				l.attach_to_parent (Void)
			end
			if n /= Void then
				n.attach_to_parent (Current)
			end
			left_child := n
		end

	put_right_child (n: like parent)
			-- Set right_child to `n'.
		require
			no_parent: n = Void or else n.is_root
		local
			c: like right_child
		do
			if n /= Void then
				if object_comparison then
					n.compare_objects
				else
					n.compare_references
				end
			end
			c := right_child
			if c /= Void then
				c.attach_to_parent (Void)
			end
			if n /= Void then
				n.attach_to_parent (Current)
			end
			right_child := n
		end

	replace (v: like item)
			-- Make `v' the cell's item.
			-- Was declared in CELL as synonym of put.
			-- (from CELL)
		require -- from TREE
			is_writable: Writable
		do
			item := v
		ensure -- from TREE
			item_inserted: item = v
		end

	replace_child (n: like new_tree)
			-- Put `n' at current child position.
			-- Was declared in FLAT_BINARY_TREE as synonym of put_child.
		require -- from TREE
			writable_child: writable_child
		do
			if object_comparison then
				n.compare_objects
			else
				n.compare_references
			end
			n.attach_to_parent (Void)
			if not has_left and not has_right then
				child_index := 1
			end
			inspect child_index
			when 1 then
				put_left_child (n)
			when 2 then
				put_right_child (n)
			end
		ensure -- from TREE
			child_replaced: child = n
		end

	sprout
			-- Make current node a root.
			-- (from TREE)
		local
			p: like parent
		do
			p := parent
			if p /= Void then
				p.prune (Current)
			end
		end

feature -- Removal

	child_remove
			-- Remove current child.
		local
			c: like left_child
		do
			inspect child_index
			when 1 then
				c := left_child
				if c /= Void then
					c.attach_to_parent (Void)
				end
				left_child := Void
			when 2 then
				c := right_child
				if c /= Void then
					c.attach_to_parent (Void)
				end
				right_child := Void
			end
		end

	forget_left
			-- Forget left sibling.
		local
			p: like parent
		do
			p := parent
			if p /= Void and then p.right_child = Current then
				p.remove_left_child
			end
		end

	forget_right
			-- Forget right sibling.
		local
			p: like parent
		do
			p := parent
			if p /= Void and then p.left_child = Current then
				p.remove_right_child
			end
		end

	prune (n: like new_tree)
			-- Prune `n' from child nodes.
		require -- from TREE
			is_child: n.parent = Current
		do
			if left_child = n then
				remove_left_child
			elseif right_child = n then
				remove_right_child
			end
		ensure -- from TREE
			n_is_root: n.is_root
		end

	remove_left_child
			-- Remove left child.
		local
			l: like left_child
		do
			l := left_child
			if l /= Void then
				l.attach_to_parent (Void)
			end
			left_child := Void
		ensure
			not has_left
		end

	remove_right_child
			-- Remove right child.
		local
			r: like right_child
		do
			r := right_child
			if r /= Void then
				r.attach_to_parent (Void)
			end
			right_child := Void
		ensure
			not has_right
		end

	wipe_out
			-- Remove all children.
		do
			remove_left_child
			remove_right_child
		ensure -- from TREE
			is_leaf: is_leaf
		end

feature -- Conversion

	binary_representation: FLAT_BINARY_TREE [G]
			-- Convert to binary tree representation:
			-- first child becomes left child,
			-- right sibling becomes right child.
			-- (from TREE)
		local
			current_sibling: FLAT_BINARY_TREE [G]
			c: like first_child
		do
			create Result.make (item)
			if not is_leaf then
				c := first_child
				if c /= Void then
					Result.put_left_child (c.binary_representation)
				end
				from
					child_start
					child_forth
					current_sibling := Result.left_child
				until
					child_after
				loop
					if current_sibling /= Void then
						c := child
						if c /= Void then
							current_sibling.put_right_child (c.binary_representation)
						end
						current_sibling := current_sibling.right_child
					end
					child_forth
				end
			end
		ensure -- from TREE
			result_is_root: Result.is_root
			result_has_no_right_child: not Result.has_right
		end

	linear_representation: FLAT_ARRAYED_LIST [G]
			-- Representation as a linear structure
			-- (from TREE)
		local
			al: FLAT_ARRAYED_LIST [G]
		do
			create al.make (count)
			al.start
			al.extend (item)
			fill_list (al)
			Result := al
		end

feature -- Duplication

	copy (other: FLAT_BINARY_TREE [G])
			-- Copy contents from `other'.
			-- (from TREE)
		local
			i: INTEGER_32
			old_idx: INTEGER_32
			tmp_tree: FLAT_BINARY_TREE [G]
			c: like child
		do
			tmp_tree := clone_node (other)
			if not other.is_leaf then
				tree_copy (other, tmp_tree)
			end
			standard_copy (tmp_tree)
			old_idx := child_index
			from
				i := 1
			until
				i > Child_capacity
			loop
				child_go_i_th (i)
				c := child
				if c /= Void then
					c.attach_to_parent (Current)
				end
				i := i + 1
			end
			child_go_i_th (old_idx)
		end

	duplicate (n: INTEGER_32): like Current
			-- Copy of sub-tree beginning at cursor position and
			-- having min (`n', arity - child_index + 1)
			-- children.
		require -- from TREE
			not_child_off: not child_off
			valid_sublist: n >= 0
		local
			c: like left_child
		do
			Result := new_tree
			c := left_child
			if child_index <= 1 and child_index + n >= 1 and c /= Void then
				Result.put_left_child (c.duplicate_all)
			end
			c := right_child
			if child_index <= 2 and child_index + n >= 2 and c /= Void then
				Result.put_right_child (c.duplicate_all)
			end
		end

	duplicate_all: like Current
		local
			c: like child
		do
			Result := new_tree
			c := left_child
			if c /= Void then
				Result.put_left_child (c.duplicate_all)
			end
			c := right_child
			if c /= Void then
				Result.put_right_child (c.duplicate_all)
			end
		end

feature {FLAT_BINARY_TREE} -- Implementation

	clone_node (n: like Current): like Current
			-- Clone node `n'.
		require -- from TREE
			not_void: n /= Void
		do
			create Result.make (n.item)
			Result.copy_node (n)
		ensure -- from TREE
			result_is_root: Result.is_root
			result_is_leaf: Result.is_leaf
		end

	copy_node (n: like Current)
			-- Copy content of `n' except tree data into Current.
		require -- from TREE
			is_root: is_root
			is_leaf: is_leaf
			not_void: n /= Void
		do
			standard_copy (n)
			child_index := 0
			left_child := Void
			right_child := Void
			parent := Void
		ensure -- from TREE
			object_comparison_copied: object_comparison = n.object_comparison
			same_arity: arity = old arity
			same_item: item = old item
			result_is_root: is_root
			result_is_leaf: is_leaf
		end

	fill_list (al: FLAT_ARRAYED_LIST [G])
			-- Fill `al' with all the children's items.
		local
			c: like left_child
		do
			c := left_child
			if c /= Void then
				al.extend (c.item)
				c.fill_list (al)
			end
			c := right_child
			if c /= Void then
				al.extend (c.item)
				c.fill_list (al)
			end
		end

feature {NONE} -- Implementation

	fill_subtree (other: FLAT_BINARY_TREE [G])
			-- Copy `other' to subtree.
		local
			c: like left_child
		do
			if attached {like Current} other as l_other then
				if not l_other.is_leaf then
					c := l_other.left_child
					if c /= Void then
						put_left_child (c.duplicate_all)
					end
				end
				if l_other.arity >= 2 then
					c := l_other.right_child
					if c /= Void then
						put_right_child (c.duplicate_all)
					end
				end
			end
		end

	new_tree: like Current
			-- New tree node
		do
			create Result.make (item)
			if object_comparison then
				Result.compare_objects
			end
		end

	remove
			-- Remove current item
			-- (from TREE)
		do
		end

	subtree_count: INTEGER_32
			-- Number of items in subtree
		local
			c: like left_child
		do
			c := left_child
			if c /= Void then
				Result := c.count
			end
			c := right_child
			if c /= Void then
				Result := Result + c.count
			end
		end

	subtree_has (v: G): BOOLEAN
			-- Does subtree contain `v'?
		local
			c: like left_child
		do
			c := left_child
			if c /= Void then
				Result := c.has (v)
			end
			c := right_child
			if c /= Void and not Result then
				Result := c.has (v)
			end
		end

	tree_copy (other, tmp_tree: FLAT_BINARY_TREE [G])
			-- Generic implementation of copy. `other' is copied onto
			-- `Current'. `tmp_tree' is used as temporary storage during
			-- copying. Since it cannot be created locally because of the
			-- generic implementation, it has to be passed in.
			-- (from TREE)
		require -- from TREE
			other_not_empty: other /= Void and then not other.is_empty
			other_not_leaf: not other.is_leaf
			tmp_tree_exists: tmp_tree /= Void
			same_rule: object_comparison = other.object_comparison
		local
			i: INTEGER_32
			p1, p2, node: FLAT_BINARY_TREE [G]
			c1: like child
			other_stack, tmp_stack: FLAT_LINKED_STACK [FLAT_BINARY_TREE [G]]
			idx_stack, orgidx_stack: FLAT_LINKED_STACK [INTEGER_32]
		do
			create other_stack.make
			create tmp_stack.make
			create idx_stack.make
			create orgidx_stack.make
			if other.object_comparison then
				tmp_tree.compare_objects
			end
			orgidx_stack.put (other.child_index)
			from
				i := 1
				p1 := other
				p2 := tmp_tree
			invariant
				same_count: other_stack.count = tmp_stack.count and tmp_stack.count = idx_stack.count
			until
				i > p1.Child_capacity and other_stack.is_empty
			loop
				p1.child_go_i_th (i)
				p2.child_go_i_th (i)
				if p1.child_readable then
					check
						source_tree_not_void: p1 /= Void
						target_tree_not_void: p2 /= Void
						source_child_not_void: p1.child /= Void
						target_child_void: p2.readable_child implies p2.child = Void
					end
					c1 := p1.child
					if c1 = Void then
						check
							source_child_not_void: p1.child /= Void
						end
					else
						node := clone_node (c1)
						check
							not_the_same: node /= p1.child
						end
						p2.put_child (node)
						check
							node_is_child: node = p2.child
							comparison_mode_ok: node.object_comparison = c1.object_comparison
							p1_consistent: c1.parent = p1
							p2_consistent: node.parent = p2
						end
						if not c1.is_leaf then
							other_stack.put (p1)
							tmp_stack.put (p2)
							idx_stack.put (i + 1)
							p1 := c1
							p2 := node
							orgidx_stack.put (p1.child_index)
							i := 0
						end
					end
				end
				if i <= p1.Child_capacity then
					i := i + 1
				else
					from
					invariant
						same_count: other_stack.count = tmp_stack.count and tmp_stack.count = idx_stack.count
					until
						other_stack.is_empty or else i <= p1.Child_capacity
					loop
						p1.child_go_i_th (orgidx_stack.item)
						p2.child_go_i_th (orgidx_stack.item)
						check
							child_indices_equal: p1.child_index = p2.child_index
						end
						p1 := other_stack.item
						p2 := tmp_stack.item
						check
							p1_not_void: p1 /= Void
							p2_not_void: p2 /= Void
						end
						i := idx_stack.item
						other_stack.remove
						tmp_stack.remove
						idx_stack.remove
						orgidx_stack.remove
					end
				end
			end
			other.child_go_i_th (orgidx_stack.item)
			tmp_tree.child_go_i_th (orgidx_stack.item)
			orgidx_stack.remove
			check
				tree_stacks_empty: other_stack.is_empty and tmp_stack.is_empty
				at_root: p1 = other and p2 = tmp_tree
				copy_correct: other ~ tmp_tree
				index_stack_empty: orgidx_stack.is_empty
			end
		end

	tree_is_equal (t1, t2: FLAT_BINARY_TREE [G]): BOOLEAN
			-- Are `t1' and `t2' recursively equal?
			-- (from TREE)
		require -- from TREE
			trees_exist: t1 /= Void and t2 /= Void
			trees_not_empty: not t1.is_empty and not t2.is_empty
			same_rule: t1.object_comparison = t2.object_comparison
		local
			p1, p2: FLAT_BINARY_TREE [G]
			c1, c2: like child
			t1_stack, t2_stack: FLAT_LINKED_STACK [FLAT_BINARY_TREE [G]]
			orgidx1_stack, orgidx2_stack: FLAT_LINKED_STACK [INTEGER_32]
			l_current_cursor, l_other_cursor: like child_cursor
		do
			l_current_cursor := t1.child_cursor
			l_other_cursor := t2.child_cursor
			if t1.is_leaf and t2.is_leaf then
				Result := t1.item ~ t2.item
			elseif t1.is_leaf xor t2.is_leaf then
				Result := False
			else
				create t1_stack.make
				create t2_stack.make
				create orgidx1_stack.make
				create orgidx2_stack.make
				orgidx1_stack.put (t1.child_index)
				orgidx2_stack.put (t2.child_index)
				from
					Result := True
					p1 := t1
					p2 := t2
					p1.child_start
					p2.child_start
				invariant
					same_count: t1_stack.count = t2_stack.count
				until
					not Result or else p1.child_after and t1_stack.is_empty
				loop
					check
						p1_not_void: p1 /= Void
						p2_not_void: p2 /= Void
					end
					if p1.child_readable and p2.child_readable and p1.Child_capacity = p2.Child_capacity then
						Result := p1.node_is_equal (p2)
						c1 := p1.child
						c2 := p2.child
						if c1 = Void or else c2 = Void then
							check
								False
							end
						else
							if not (c1.is_leaf or c2.is_leaf) then
								t1_stack.put (p1)
								t2_stack.put (p2)
								p1 := c1
								p2 := c2
								Result := p1.node_is_equal (p2)
								orgidx1_stack.put (p1.child_index)
								orgidx2_stack.put (p2.child_index)
								p1.child_start
								p2.child_start
							elseif c1.is_leaf xor c2.is_leaf then
								Result := False
							else
								Result := c1.node_is_equal (c2)
							end
						end
					elseif p1.Child_capacity /= p2.Child_capacity or else (p1.child_readable xor p2.child_readable) then
						Result := False
					end
					if not p1.child_after then
						p1.child_forth
						p2.child_forth
					else
						from
						invariant
							same_count: t1_stack.count = t2_stack.count
						until
							t1_stack.is_empty or else not p1.child_after
						loop
							p1 := t1_stack.item
							p2 := t2_stack.item
							p1.child_forth
							p2.child_forth
							t1_stack.remove
							t2_stack.remove
							orgidx1_stack.remove
							orgidx2_stack.remove
						end
					end
				end
				if not Result then
					from
					invariant
						same_count: t1_stack.count = t2_stack.count and orgidx1_stack.count = orgidx2_stack.count
					until
						orgidx1_stack.count = 1
					loop
						p1.child_go_i_th (orgidx1_stack.item)
						p2.child_go_i_th (orgidx2_stack.item)
						p1 := t1_stack.item
						p2 := t2_stack.item
						check
							p1_not_void: p1 /= Void
							p2_not_void: p2 /= Void
						end
						t1_stack.remove
						t2_stack.remove
						orgidx1_stack.remove
						orgidx2_stack.remove
					end
					check
						tree_stacks_empty: t1_stack.is_empty and t2_stack.is_empty
						at_root: p1 = t1 and p2 = t2
						p1_not_void: p1 /= Void
						p2_not_void: p2 /= Void
					end
					p1.child_go_i_th (orgidx1_stack.item)
					p2.child_go_i_th (orgidx2_stack.item)
					orgidx1_stack.remove
					orgidx2_stack.remove
					check
						index_stacks_empty: orgidx1_stack.is_empty and orgidx2_stack.is_empty
					end
				end
			end
			t1.child_go_to (l_current_cursor)
			t2.child_go_to (l_other_cursor)
		end

feature {FLAT_BINARY_TREE} -- Implementation

	attach_to_parent (n: like parent)
			-- Make `n' parent of current node.
			-- (from TREE)
		do
			parent := n
		ensure -- from TREE
			new_parent: parent = n
		end

invariant
	tree_is_binary: Child_capacity = 2

		-- from TREE
	tree_consistency: child_readable implies (attached child as c and then c.parent = Current)
	leaf_definition: is_leaf = (arity = 0)
	child_off_definition: child_off = child_before or child_after
	child_before_definition: child_before = (child_index = 0)
	child_isfirst_definition: child_isfirst = (not is_leaf and child_index = 1)
	child_islast_definition: child_islast = (not is_leaf and child_index = Child_capacity)
	child_after_definition: child_after = (child_index >= Child_capacity + 1)

end -- class BINARY_TREE

