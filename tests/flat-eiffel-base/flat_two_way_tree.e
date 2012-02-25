class
	FLAT_TWO_WAY_TREE [G]

inherit
	FLAT_READABLE_INDEXABLE [G]
		rename
			item as i_th alias "[]"
		redefine
			copy,
			is_equal
		end

	FLAT_BI_LINKABLE [G]
		rename
			left as left_sibling,
			right as right_sibling,
			put_left as bl_put_left,
			put_right as bl_put_right
		export
			{ANY}
				left_sibling, right_sibling
			{FLAT_TWO_WAY_TREE}
				bl_put_left, bl_put_right,
				forget_left, forget_right
		undefine
			copy, is_equal
		end

create
	make

feature -- Initialization

	make (v: like item)
			-- Create single node with item `v'.
		do
			put (v)
			twl_make
		end

feature {NONE} -- Initialization

	twl_make
			-- Create an empty list.
			-- (from LINKED_LIST)
		do
			child_before := True
		ensure -- from LINKED_LIST
			is_before: child_before
		end

feature -- Access

	at alias "@" (i: INTEGER_32): like child_item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in CHAIN as synonym of i_th.
			-- (from CHAIN)
		require -- from TABLE
			valid_key: valid_index (i)
		local
			pos: FLAT_CURSOR
		do
			pos := child_cursor
			child_go_i_th (i)
			Result := child_item
			child_go_to (pos)
		end

	child_cursor: FLAT_TWO_WAY_TREE_CURSOR [G]
			-- Current cursor position
		do
			create Result.make (child, child_after, child_before)
		ensure -- from CURSOR_STRUCTURE
			cursor_not_void: Result /= Void
		end

	first: like child_item
			-- Item at first position
			-- (from LINKED_LIST)
		require -- from CHAIN
			not_empty: not is_leaf
		local
			f: like first_child
		do
			f := first_child
			check
				f_attached: f /= Void
			end
			Result := f.item
		end

	first_child: like parent
			-- Leftmost child

	i_th alias "[]" (i: INTEGER_32): like child_item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in CHAIN as synonym of at.
			-- (from CHAIN)
		local
			pos: FLAT_CURSOR
		do
			pos := child_cursor
			child_go_i_th (i)
			Result := child_item
			child_go_to (pos)
		end

	child_index: INTEGER_32
			-- Index of current position
			-- (from LINKED_LIST)
		local
			l_active, l_active_iterator: like child
		do
			if child_after then
				Result := arity + 1
			elseif not child_before then
				from
					Result := 1
					l_active := child
					l_active_iterator := first_child
				until
					l_active_iterator = l_active or else l_active_iterator = Void
				loop
					l_active_iterator := l_active_iterator.right_sibling
					Result := Result + 1
				end
			end
		ensure -- from TREE
			valid_index: Result >= 0 and Result <= arity + 1
		end

	index_of (v: like child_item; i: INTEGER_32): INTEGER_32
			-- Index of `i'-th occurrence of item identical to `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- 0 if none.
			-- (from CHAIN)
		require -- from LINEAR
			positive_occurrences: i > 0
		local
			pos: FLAT_CURSOR
		do
			pos := child_cursor
			Result := sequential_index_of (v, i)
			child_go_to (pos)
		ensure -- from LINEAR
			non_negative_result: Result >= 0
		end

	child_item: G
			-- Current item
			-- (from LINKED_LIST)
		require -- from TRAVERSABLE
			not_off: not child_off
		local
			a: like child
		do
			a := child
			check
				a_attached: a /= Void
			end
			Result := a.item
		end

	item_for_iteration: G
			-- Item at current position
			-- (from LINEAR)
		require -- from LINEAR
			not_off: not child_off
		do
			Result := child_item
		end

	last: like child_item
			-- Item at last position
			-- (from LINKED_LIST)
		require -- from CHAIN
			not_empty: not is_leaf
		local
			l: like last_child
		do
			l := last_child
			check
				l_attached: l /= Void
			end
			Result := l.item
		end

	last_child: like parent
			-- Right most child

	parent: like new_cell
			-- Parent node

	sublist: FLAT_TWO_WAY_TREE [G]
			-- Result produced by last split
			-- (from TWO_WAY_LIST)

feature {NONE} -- Access

	twl_has (v: like child_item): BOOLEAN
			-- Does chain include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from CHAIN)
		local
			pos: FLAT_CURSOR
		do
			pos := child_cursor
			Result := sequential_has (v)
			child_go_to (pos)
		ensure -- from CONTAINER
			not_found_in_empty: Result implies not is_leaf
		end

	sequential_has (v: like child_item): BOOLEAN
			-- Does structure include an occurrence of `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		require -- from  CONTAINER
			True
		do
			child_start
			if not child_off then
				search_child (v)
			end
			Result := not exhausted
		ensure -- from CONTAINER
			not_found_in_empty: Result implies not is_leaf
		end

	sequential_index_of (v: like child_item; i: INTEGER_32): INTEGER_32
			-- Index of `i'-th occurrence of `v'.
			-- 0 if none.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		require -- from LINEAR
			positive_occurrences: i > 0
		local
			occur, pos: INTEGER_32
		do
			if object_comparison and v /= Void then
				from
					child_start
					pos := 1
				until
					exhausted or (occur = i)
				loop
					if child_item ~ v then
						occur := occur + 1
					end
					child_forth
					pos := pos + 1
				end
			else
				from
					child_start
					pos := 1
				until
					exhausted or (occur = i)
				loop
					if child_item = v then
						occur := occur + 1
					end
					child_forth
					pos := pos + 1
				end
			end
			if occur = i then
				Result := pos - 1
			end
		ensure -- from LINEAR
			non_negative_result: Result >= 0
		end

	twl_new_cursor: FLAT_TWO_WAY_TREE_ITERATION_CURSOR [G]
			-- Fresh cursor associated with current structure
			-- (from TWO_WAY_LIST)
		require -- from  ITERABLE
			True
		do
			create Result.make (Current)
			Result.start
		ensure -- from ITERABLE
			result_attached: Result /= Void
		end

	sequential_occurrences (v: like child_item): INTEGER_32
			-- Number of times `v' appears.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		require -- from  BAG
			True
		do
			from
				child_start
				search_child (v)
			until
				exhausted
			loop
				Result := Result + 1
				child_forth
				search_child (v)
			end
		ensure -- from BAG
			non_negative_occurrences: Result >= 0
		end

	sequential_search (v: like child_item)
			-- Move to first position (at or after current
			-- position) where item and `v' are equal.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- If no such position ensure that exhausted will be true.
			-- (from LINEAR)
		do
			if object_comparison then
				from
				until
					exhausted or else v ~ child_item
				loop
					child_forth
				end
			else
				from
				until
					exhausted or else v = child_item
				loop
					child_forth
				end
			end
		ensure -- from LINEAR
			object_found: (not exhausted and object_comparison) implies v ~ child_item
			item_found: (not exhausted and not object_comparison) implies v = child_item
		end

feature -- Measurement

	child_capacity: INTEGER_32
			-- Maximal number of children
			-- (from TREE)
		do
			Result := arity
		end

	count: INTEGER_32
			-- Number of items
			-- (from TREE)
		do
			Result := subtree_count + 1
		end

	arity: INTEGER_32
			-- Number of items
			-- (from LINKED_LIST)

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
			-- (from CHAIN)
		do
			create Result.make (1, arity)
		ensure then -- from CHAIN
			count_definition: Result.count = arity
		end

	occurrences (v: like child_item): INTEGER_32
			-- Number of times `v' appears.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from CHAIN)
		local
			pos: FLAT_CURSOR
		do
			pos := child_cursor
			Result := sequential_occurrences (v)
			child_go_to (pos)
		ensure -- from BAG
			non_negative_occurrences: Result >= 0
		end

feature -- Comparison

	is_equal (other: FLAT_TWO_WAY_TREE [G]): BOOLEAN
			-- Does `other' contain the same elements?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from TREE)
		do
			if Current = other then
				Result := True
			else
				Result := (is_leaf = other.is_leaf) and (object_comparison = other.object_comparison) and (child_capacity = other.child_capacity)
				if Result and not is_leaf then
					Result := tree_is_equal (Current, other)
				end
			end
		ensure then -- from LIST
			indices_unchanged: child_index = old child_index and other.child_index = old other.child_index
			true_implies_same_size: Result implies arity = other.arity
		end

	node_is_equal (other: FLAT_TWO_WAY_TREE [G]): BOOLEAN
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

	child_after: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?
			-- (from LINKED_LIST)

	child_before: BOOLEAN
			-- Is there no valid cursor position to the left of cursor?
			-- (from LINKED_LIST)

	changeable_comparison_criterion: BOOLEAN
			-- May object_comparison be changed?
			-- (Answer: yes by default.)
			-- (from CONTAINER)
		do
			Result := True
		end

	child_isfirst: BOOLEAN
			-- Is cursor under first child?
			-- (from TREE)
		require -- from  CHAIN
			True
		do
			Result := not is_leaf and child_index = 1
		ensure -- from CHAIN
			valid_position: Result implies not is_leaf
		end

	child_islast: BOOLEAN
			-- Is cursor under last child?
		do
			Result := not is_leaf and ((child = last_child)
				and then not child_after
				and then not child_before)
		ensure -- from CHAIN
			valid_position: Result implies not is_leaf
		end

	child_readable: BOOLEAN
			-- Is there a current child_item to be read?
			-- (from TREE)
		require -- from  ACTIVE
			True
		do
			Result := not child_off and then (child /= Void)
		end

	child_writable: BOOLEAN
			-- Is there a current child_item that may be modified?
			-- (from TREE)
		require -- from  ACTIVE
			True
		do
			Result := not child_off and then (child /= Void)
		end

	exhausted: BOOLEAN
			-- Has structure been completely explored?
			-- (from LINEAR)
		do
			Result := child_off
		ensure -- from LINEAR
			exhausted_when_off: child_off implies Result
		end

	Extendible: BOOLEAN = True
			-- May new items be added?
			-- (from DYNAMIC_TREE)

	child_extendible: BOOLEAN
			-- May new items be added? (Answer: yes.)
			-- (from DYNAMIC_CHAIN)
		require -- from  COLLECTION
			True
		do
			Result := True
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
			not_found_in_empty: Result implies not is_leaf
		end

	is_empty: BOOLEAN
			-- Is structure empty of items?
			-- (from TREE)
		do
			Result := False
		end

	is_inserted (v: G): BOOLEAN
			-- Has `v' been inserted at the end by the most recent put or
			-- extend?
			-- (from LINKED_LIST)
		local
			l: like last_child
		do
			l := last_child
			if l /= Void then
				check
					put_constraint: (v /= l.item) implies not child_off
				end
				Result := (v = l.item) or else (v = child_item)
			end
		end

	is_leaf: BOOLEAN
			-- Are there no children?
			-- (from TREE)
		require -- from  CONTAINER
			True
		do
			Result := arity = 0
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

	child_off: BOOLEAN
			-- Is there no current item?
			-- (from LINKED_LIST)
		do
			Result := child_after or child_before
		end

	prunable: BOOLEAN
			-- May items be removed? (Answer: yes.)
			-- (from DYNAMIC_CHAIN)
		do
			Result := True
		end

	Readable: BOOLEAN = True
			-- (from TREE)

	readable_child: BOOLEAN
			-- Is there a current child to be read?
			-- (from TREE)
		do
			Result := not child_off
		end

	replaceable: BOOLEAN
			-- Can current item be replaced?
			-- (from ACTIVE)
		do
			Result := True
		end

	valid_cursor (p: FLAT_CURSOR): BOOLEAN
			-- Can the cursor be moved to position `p'?
			-- (from LINKED_LIST)
		local
			temp, sought: like first_child
		do
			if attached {like child_cursor} p as ll_c then
				from
					temp := first_child
					sought ?= ll_c.active
					Result := ll_c.after or else ll_c.before
				until
					Result or else temp = Void
				loop
					Result := (temp = sought)
					temp := temp.right_sibling
				end
			end
		end

	valid_cursor_index (i: INTEGER_32): BOOLEAN
			-- Is `i' correctly bounded for cursor movement?
			-- (from TREE)
		require -- from  CHAIN
			True
		do
			Result := (i >= 0) and (i <= child_capacity + 1)
		ensure -- from CHAIN
			valid_cursor_index_definition: Result = ((i >= 0) and (i <= arity + 1))
--		ensure -- from TREE
			valid_cursor_index_definition: Result = (i >= 0) and (i <= child_capacity + 1)
		end

	valid_index (i: INTEGER_32): BOOLEAN
			-- Is `i' within allowable bounds?
			-- (from CHAIN)
		do
			Result := (i >= 1) and (i <= arity)
		ensure then -- from CHAIN
			valid_index_definition: Result = ((i >= 1) and (i <= arity))
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

feature {NONE} -- Status report

	Twl_full: BOOLEAN = False
			-- Is structured filled to capacity? (Answer: no.)
			-- (from LINKED_LIST)

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
			-- Move cursor to previous position, if any.
			-- (from TWO_WAY_LIST)
		require -- from BILINEAR
			not_before: not child_before
		local
			a: like child
		do
			if child_after then
				child_after := False
				if is_leaf then
					child_before := True
				end
			else
				a := child
				if a /= Void then
					a := a.left_sibling
					if a = Void then
						child := first_child
						child_before := True
					else
						child := a
					end
				end
			end
		end

	child_finish
			-- Move cursor to last position.
			-- (Go before if empty)
			-- (from TWO_WAY_LIST)
		do
			if not is_leaf then
				child := last_child
				child_after := False
				child_before := False
			else
				child_after := False
				child_before := True
			end
		ensure then -- from CHAIN
			at_last: not is_leaf implies child_islast
--		ensure then -- from TWO_WAY_LIST
			not_after: not child_after
		end

	child_forth
			-- Move cursor to next position, if any.
			-- (from TWO_WAY_LIST)
		require -- from LINEAR
			not_after: not child_after
		local
			a: like child
		do
			if child_before then
				child_before := False
				if is_leaf then
					child_after := True
				end
			else
				a := child
				if a /= Void then
					a := a.right_sibling
					if a = Void then
						child := last_child
						child_after := True
					else
						child := a
					end
				end
			end
		ensure then -- from LIST
			moved_forth: child_index = old child_index + 1
		end

	child_go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th position.
			-- (from LINKED_LIST)
		require -- from CHAIN
			valid_cursor_index: valid_cursor_index (i)
		do
			if i = 0 then
				child_before := True
				child_after := False
				child := first_child
			elseif i = arity + 1 then
				child_before := False
				child_after := True
				child := last_child
			else
				move (i - child_index)
			end
		ensure -- from CHAIN
			position_expected: child_index = i
		end

	child_go_to (p: FLAT_CURSOR)
			-- Move cursor to position `p'.
			-- (from LINKED_LIST)
		require -- from CURSOR_STRUCTURE
			cursor_position_valid: valid_cursor (p)
		do
			if attached {like child_cursor} p as ll_c then
				child_after := ll_c.after
				child_before := ll_c.before
				if child_before then
					child := first_child
				elseif child_after then
					child := last_child
				else
					child ?= ll_c.active
				end
			else
				check
					correct_cursor_type: False
				end
			end
		end

	move (i: INTEGER_32)
			-- Move cursor `i' positions. The cursor
			-- may end up `off' if the offset is to big.
			-- (from TWO_WAY_LIST)
		require -- from  CHAIN
			True
		local
			counter: INTEGER_32
			p: like first_child
		do
			if i > 0 then
				ll_move (i)
			elseif i < 0 then
				if child_after then
					child_after := False
					counter := -1
				end
				from
					p := child
				until
					(counter = i) or else (p = Void)
				loop
					p := p.left_sibling
					counter := counter - 1
				end
				if p = Void then
					child_before := True
					child := first_child
				else
					child := p
				end
			end
		ensure -- from CHAIN
			too_far_right: (old child_index + i > arity) implies exhausted
			too_far_left: (old child_index + i < 1) implies exhausted
			expected_index: (not exhausted) implies (child_index = old child_index + i)
--		ensure then -- from LINKED_LIST
			moved_if_inbounds: ((old child_index + i) >= 0 and (old child_index + i) <= (arity + 1)) implies child_index = (old child_index + i)
			before_set: (old child_index + i) <= 0 implies child_before
			after_set: (old child_index + i) >= (arity + 1) implies child_after
		end

	search_child (v: like child_item)
			-- Move to first position (at or after current
			-- position) where item and `v' are equal.
			-- If structure does not include `v' ensure that
			-- exhausted will be true.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from BILINEAR)
		do
			if child_before and not is_leaf then
				child_forth
			end
			sequential_search (v)
		ensure -- from LINEAR
			object_found: (not exhausted and object_comparison) implies v ~ child_item
			item_found: (not exhausted and not object_comparison) implies v = child_item
		end

	child_start
			-- Move cursor to first position.
			-- (from LINKED_LIST)
		do
			if first_child /= Void then
				child := first_child
				child_after := False
			else
				child_after := True
			end
			child_before := False
		ensure then -- from CHAIN
			at_first: not is_leaf implies child_isfirst
--		ensure then -- from LINKED_LIST
			empty_convention: is_leaf implies child_after
		end

feature {NONE} -- Cursor movement

	ll_move (i: INTEGER_32)
			-- Move cursor `i' positions. The cursor
			-- may end up `off' if the offset is too big.
			-- (from LINKED_LIST)
		local
			counter, new_index: INTEGER_32
			p: like first_child
		do
			if i > 0 then
				if child_before then
					child_before := False
					counter := 1
				end
				from
					p := child
				until
					(counter = i) or else (p = Void)
				loop
					child := p
					p := p.right_sibling
					counter := counter + 1
				end
				if p = Void then
					child_after := True
				else
					child := p
				end
			elseif i < 0 then
				new_index := child_index + i
				child_before := True
				child_after := False
				child := first_child
				if (new_index > 0) then
					move (new_index)
				end
			end
		ensure -- from CHAIN
			too_far_right: (old child_index + i > arity) implies exhausted
			too_far_left: (old child_index + i < 1) implies exhausted
			expected_index: (not exhausted) implies (child_index = old child_index + i)
--		ensure then -- from LINKED_LIST
			moved_if_inbounds: ((old child_index + i) >= 0 and (old child_index + i) <= (arity + 1)) implies child_index = (old child_index + i)
			before_set: (old child_index + i) <= 0 implies child_before
			after_set: (old child_index + i) >= (arity + 1) implies child_after
		end

feature {FLAT_TWO_WAY_TREE} -- Element change

	set_child (n: like parent)
			-- Set the child of parent to `n'.
		require -- from DYNAMIC_TREE
			non_void_argument: n /= Void
		do
			child := n
		ensure then
			child_set: child = n
		end

feature -- Element change

	append (s: FLAT_TWO_WAY_TREE [G])
			-- Append a copy of `s'.
			-- (from CHAIN)
		require -- from SEQUENCE
			argument_not_void: s /= Void
		local
			l: like s
			l_cursor: FLAT_CURSOR
		do
			l := s
			if s = Current then
				l := twin
			end
			from
				l_cursor := child_cursor
				l.child_start
			until
				l.exhausted
			loop
				child_extend (l.item)
				child_finish
				l.child_forth
			end
			child_go_to (l_cursor)
		ensure -- from SEQUENCE
			new_count: arity >= old arity
		end

	extend (v: like item)
			-- Add `v' as new child.
			-- (from DYNAMIC_TREE)
		do
			child_extend (v)
		end

	child_extend (v: like child_item)
			-- Add `v' to end.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
		require -- from COLLECTION
			extendible: child_extendible
		local
			p: like first_child
		do
			p := new_cell (v)
			if is_leaf then
				first_child := p
				child := p
			else
				p.bl_put_left (last_child)
			end
			last_child := p
			if child_after then
				child := p
			end
			arity := arity + 1
		ensure -- from COLLECTION
			item_inserted: is_inserted (v)
		end

	fill (other: FLAT_TWO_WAY_TREE [G])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current node
			-- need not be the same.
			-- (from TREE)
		do
			replace (other.item)
			fill_subtree (other)
		end

	force (v: like child_item)
			-- Add `v' to end.
			-- (from SEQUENCE)
		require -- from SEQUENCE
			extendible: child_extendible
		do
			child_extend (v)
		ensure then -- from SEQUENCE
			new_count: arity = old arity + 1
			item_inserted: has (v)
		end

	twl_merge_left (other: FLAT_TWO_WAY_TREE [G])
			-- Merge `other' into current structure before cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from TWO_WAY_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: child_extendible
			not_before: not child_before
			other_exists: other /= Void
			not_current: other /= Current
		local
			other_first_element: like first_child
			other_last_element: like first_child
			other_count: INTEGER_32
			a: like child
		do
			if not other.is_leaf then
				other_first_element := other.first_child
				other_last_element := other.last_child
				other_count := other.arity
				other.wipe_out
				check
					other_first_element /= Void
					other_last_element /= Void
				end
				if is_leaf then
					last_child := other_last_element
					first_child := other_first_element
					if child_before then
						child := first_child
					else
						child := last_child
					end
				elseif child_isfirst then
					other_last_element.bl_put_right (first_child)
					first_child := other_first_element
				elseif child_after then
					other_first_element.bl_put_left (last_child)
					last_child := other_last_element
					child := last_child
				else
					a := child
					if a /= Void then
						other_first_element.bl_put_left (a.left_sibling)
						a.bl_put_left (other_last_element)
					end
				end
				arity := arity + other_count
			end
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity + old other.arity
			new_index: child_index = old child_index + old other.arity
			other_is_empty: other.is_leaf
		end

	twl_merge_right (other: FLAT_TWO_WAY_TREE [G])
			-- Merge `other' into current structure after cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from TWO_WAY_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: child_extendible
			not_after: not child_after
			other_exists: other /= Void
			not_current: other /= Current
		do
			if is_leaf or else child_islast then
				last_child := other.last_child
			end
			ll_merge_right (other)
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity + old other.arity
			same_index: child_index = old child_index
			other_is_empty: other.is_leaf
		end

	merge_tree_after (other: like new_cell)
			-- Merge children of `other' into current structure
			-- after cursor position. Do not move cursor.
			-- Make `other' a leaf.
		require -- from DYNAMIC_TREE
			not_child_off: not child_off
			other_exists: (other /= Void)
		do
			attach (other)
			twl_merge_right (other)
		ensure -- from DYNAMIC_TREE
			other_is_leaf: other.is_leaf
		end

	merge_tree_before (other: like new_cell)
			-- Merge children of `other' into current structure
			-- before cursor position. Do not move cursor.
			-- Make `other' a leaf.
		require -- from DYNAMIC_TREE
			not_child_off: not child_off
			other_exists: (other /= Void)
		do
			attach (other)
			twl_merge_left (other)
		ensure -- from DYNAMIC_TREE
			other_is_leaf: other.is_leaf
		end

	prune (n: like new_cell)
			-- Prune `n' from children.
		require -- from TREE
			is_child: n.parent = Current
		local
			l_child: like first_child
			c: like child
		do
			from
				l_child := first_child
			until
				l_child = Void or l_child = n
			loop
				l_child := l_child.right_sibling
			end
			if l_child /= Void then
				if l_child = first_child then
					first_child := l_child.right_sibling
					if child = n then
						child := first_child
					end
					if l_child = last_child then
						last_child := l_child.left_sibling
					end
				elseif l_child = last_child then
					last_child := l_child.left_sibling
					if child = n then
						child := last_child
					end
				else
					c := l_child.right_sibling
					if c /= Void then
						c.bl_put_left (l_child.left_sibling)
					end
					if child = n then
						child := l_child.left_sibling
					end
				end
				arity := arity - 1
				if is_leaf and not child_before then
					first_child := Void
					last_child := Void
					child_after := True
				end
				n.attach_to_parent (Void)
				n.simple_forget_left
				n.simple_forget_right
			end
		ensure -- from TREE
			n_is_root: n.is_root
		end

	child_put (v: like child_item)
			-- Replace current item by `v'.
			-- (Synonym for replace)
			-- (from CHAIN)
		require -- from TREE
			child_writable: child_writable
--		require -- from CHAIN
--			writeable: child_writable
--			replaceable: replaceable
		do
			child_replace (v)
		ensure -- from TREE
			item_inserted: child_item = v
--		ensure -- from CHAIN
			same_count: arity = old arity
			is_inserted: is_inserted (v)
		end

	sequence_put (v: like child_item)
			-- Add `v' to end.
			-- (from SEQUENCE)
		require -- from COLLECTION
			extendible: child_extendible
		do
			child_extend (v)
		ensure -- from COLLECTION
			item_inserted: is_inserted (v)
--		ensure then -- from SEQUENCE
			new_count: arity = old arity + 1
		end

	put_child (n: like new_cell)
			-- Add `n' to the list of children.
			-- Do not move child cursor.
		require -- from TREE
			non_void_argument: n /= Void
		local
			c: like last_child
		do
			if object_comparison then
				n.compare_objects
			else
				n.compare_references
			end
			if is_leaf then
				first_child := n
				child := n
			else
				c := last_child
				if c /= Void then
					c.bl_put_right (n)
				end
				if child_after then
					child := n
				end
			end
			last_child := n
			n.attach_to_parent (Current)
			arity := arity + 1
		end

	put_child_left (n: like new_cell)
			-- Add `n' to the left of cursor position.
			-- Do not move cursor.
		require -- from DYNAMIC_TREE
			not_child_before: not child_before
			non_void_argument: n /= Void
		do
			child_back
			put_child_right (n)
			child_forth
			child_forth
		end

	put_child_right (n: like new_cell)
			-- Add `n' to the right of cursor position.
			-- Do not move cursor.
		require -- from DYNAMIC_TREE
			not_child_after: not child_after
			non_void_argument: n /= Void
		local
			c: like child
		do
			if object_comparison then
				n.compare_objects
			else
				n.compare_references
			end
			if child_before then
				if is_leaf then
					last_child := n
				end
				n.bl_put_right (first_child)
				first_child := n
				child := n
			elseif child_islast then
				c := child
				if c /= Void then
					c.bl_put_right (n)
				end
				last_child := n
			else
				c := child
				if c /= Void then
					n.bl_put_right (c.right_sibling)
				end
				n.bl_put_left (c)
			end
			n.attach_to_parent (Current)
			arity := arity + 1
		end

	put_front (v: like child_item)
			-- Add `v' to beginning.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: child_extendible
		do
			ll_put_front (v)
			if arity = 1 then
				last_child := first_child
			end
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity + 1
			item_inserted: first = v
		end

	put_i_th (v: like child_item; i: INTEGER_32)
			-- Put `v' at `i'-th position.
			-- (from CHAIN)
		require -- from TABLE
			valid_key: valid_index (i)
		local
			pos: FLAT_CURSOR
		do
			pos := child_cursor
			child_go_i_th (i)
			child_replace (v)
			child_go_to (pos)
		ensure -- from TABLE
			inserted: i_th (i) = v
		end

	child_put_left (v: like child_item)
			-- Add `v' to the left of cursor position.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
--		require -- from DYNAMIC_CHAIN
--			extendible: child_extendible
--			not_before: not child_before
		require -- from DYNAMIC_TREE
			not_child_before: not child_before
		local
			p: like first_child
			a: like child
		do
			p := new_cell (v)
			if is_leaf then
				first_child := p
				last_child := p
				child := p
				child_before := False
			elseif child_after then
				p.bl_put_left (last_child)
				last_child := p
				child := p
			elseif child_isfirst then
				p.bl_put_right (child)
				first_child := p
			else
				a := child
				if a /= Void then
					p.bl_put_left (a.left_sibling)
				end
				p.bl_put_right (child)
			end
			arity := arity + 1
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity + 1
			new_index: child_index = old child_index + 1
--		ensure then -- from LINKED_LIST
			previous_exists: previous /= Void
			item_inserted: attached previous as q and then q.item = v
		end

	child_put_right (v: like child_item)
			-- Add `v' to the right of cursor position.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
--		require -- from DYNAMIC_CHAIN
--			extendible: child_extendible
--			not_after: not child_after
		require -- from DYNAMIC_TREE
			not_child_after: not child_after
		local
			was_last: BOOLEAN
			a: like child
		do
			was_last := child_islast
			ll_put_right (v)
			if arity = 1 then
				last_child := child
			elseif was_last then
				a := child
				if a /= Void then
					last_child := a.right_sibling
				end
			end
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity + 1
			same_index: child_index = old child_index
--		ensure then -- from LINKED_LIST
			next_exists: next /= Void
			item_inserted: not old child_before implies (attached next as n and then n.item = v)
			item_inserted_before: old child_before implies (attached child as c and then c.item = v)
		end

	child_replace (v: like child_item)
			-- Replace current item by `v'.
			-- (from LINKED_LIST)
--		require -- from ACTIVE
--			writable: child_writable
--			replaceable: replaceable
		require -- from TREE
			child_writable: child_writable
		local
			a: like child
		do
			a := child
			if a /= Void then
				a.put (v)
			end
		ensure -- from ACTIVE
			item_replaced: child_item = v
		end

	replace_child (n: like new_cell)
			-- Replace current child by `n'.
		require -- from TREE
			writable_child: writable_child
		do
			remove_child
			put_child_right (n)
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

feature {NONE} -- Element change

	twl_fill (other: FLAT_TWO_WAY_TREE [G])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current structure
			-- need not be the same.
			-- (from CHAIN)
		require -- from COLLECTION
			other_not_void: other /= Void
			extendible: child_extendible
		local
			lin_rep: FLAT_ARRAYED_LIST [G]
			l_cursor: FLAT_CURSOR
		do
			lin_rep := other.linear_representation
			from
				l_cursor := child_cursor
				lin_rep.start
			until
				not child_extendible or else lin_rep.off
			loop
				child_extend (lin_rep.item)
				child_finish
				lin_rep.forth
			end
			child_go_to (l_cursor)
		end

	ll_merge_right (other: FLAT_TWO_WAY_TREE [G])
			-- Merge `other' into current structure after cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: child_extendible
			not_after: not child_after
			other_exists: other /= Void
			not_current: other /= Current
		local
			other_first_element: like first_child
			other_last_element: like last_child
			other_count: INTEGER_32
			a: like child
		do
			other_last_element := other.last_child
			if other_last_element /= Void then
				other_first_element := other.first_child
				other_count := other.arity
				other.wipe_out
				check
					other_first_element /= Void
					other_last_element /= Void
				end
				a := child
				if a = Void then
					first_child := other_first_element
					child := first_child
				else
					if not child_islast then
						other_last_element.bl_put_right (a.right_sibling)
					end
					a.bl_put_right (other_first_element)
				end
				arity := arity + other_count
			end
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity + old other.arity
			same_index: child_index = old child_index
			other_is_empty: other.is_leaf
		end

	ll_put_front (v: like child_item)
			-- Add `v' to beginning.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: child_extendible
		local
			p: like new_cell
		do
			p := new_cell (v)
			p.bl_put_right (first_child)
			first_child := p
			if child_before or is_leaf then
				child := p
			end
			arity := arity + 1
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity + 1
			item_inserted: first = v
		end

	ll_put_right (v: like child_item)
			-- Add `v' to the right of cursor position.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: child_extendible
			not_after: not child_after
		local
			p: like new_cell
			a: like child
		do
			p := new_cell (v)
			check
				is_leaf implies child_before
			end
			if child_before then
				p.bl_put_right (first_child)
				first_child := p
				child := p
			else
				a := child
				if a /= Void then
					p.bl_put_right (a.right_sibling)
					a.bl_put_right (p)
				end
			end
			arity := arity + 1
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity + 1
			same_index: child_index = old child_index
--		ensure then -- from LINKED_LIST
			next_exists: next /= Void
			item_inserted: not old child_before implies (attached next as n and then n.item = v)
			item_inserted_before: old child_before implies (attached child as c and then c.item = v)
		end

feature -- Removal

	twl_prune (v: like child_item)
			-- Remove first occurrence of `v', if any,
			-- after cursor position.
			-- If found, move cursor to right neighbor;
			-- if not, make structure exhausted.
			-- (from DYNAMIC_CHAIN)
		require -- from COLLECTION
			prunable: prunable
		do
			search_child (v)
			if not exhausted then
				remove_child
			end
		end

	prune_all (v: like child_item)
			-- Remove all occurrences of `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- Leave structure exhausted.
			-- (from DYNAMIC_CHAIN)
		require -- from COLLECTION
			prunable: prunable
		do
			from
				child_start
				search_child (v)
			until
				exhausted
			loop
				remove_child
				search_child (v)
			end
		ensure -- from COLLECTION
			no_more_occurrences: not has (v)
--		ensure then -- from DYNAMIC_CHAIN
			is_exhausted: exhausted
		end

	remove_child
			-- Remove current item.
			-- Move cursor to right neighbor
			-- (or `after' if no right neighbor).
			-- (from TWO_WAY_LIST)
--		require -- from ACTIVE
--			prunable: prunable
--			writable: child_writable
		require -- from DYNAMIC_TREE
			child_not_off: not child_off
		local
			succ, pred, removed: like first_child
			a: like child
		do
			removed := child
			if child_isfirst then
				a := first_child
				if a /= Void then
					child := a.right_sibling
					a.forget_right
				end
				first_child := child
				if arity = 1 then
					check
						no_active: child = Void
					end
					child_after := True
					last_child := Void
				end
			elseif child_islast then
				a := last_child
				if a /= Void then
					child := a.left_sibling
					a.forget_left
				end
				last_child := child
				child_after := True
			else
				a := child
				if a /= Void then
					pred := a.left_sibling
					succ := a.right_sibling
				end
				if pred /= Void and then succ /= Void then
					pred.forget_right
					succ.forget_left
					pred.bl_put_right (succ)
				end
				child := succ
			end
			arity := arity - 1
			cleanup_after_remove (removed)
		ensure then -- from DYNAMIC_LIST
			after_when_empty: is_leaf implies child_after
--		ensure -- from DYNAMIC_TREE
			new_arity: arity = old arity - 1
			new_child_index: child_index = old child_index
		end

	remove_left_child
			-- Remove item to the left of cursor position.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
--		require -- from DYNAMIC_CHAIN
--			left_exists: child_index > 1
		require -- from DYNAMIC_TREE
			is_not_first: not child_isfirst
		do
			child_back
			remove_child
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity - 1
			new_index: child_index = old child_index - 1
--		ensure -- from DYNAMIC_TREE
			new_arity: arity = old arity - 1
			new_child_index: child_index = old child_index - 1
		end

	remove_right_child
			-- Remove item to the right of cursor position.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
--		require -- from DYNAMIC_CHAIN
--			right_exists: child_index < arity
		require -- from DYNAMIC_TREE
			is_not_last: not child_islast
		do
			child_forth
			remove_child
			child_back
		ensure -- from DYNAMIC_CHAIN
			new_count: arity = old arity - 1
			same_index: child_index = old child_index
--		ensure -- from DYNAMIC_TREE
			new_arity: arity = old arity - 1
			new_child_index: child_index = old child_index
		end

	remove_sublist
			-- (from TWO_WAY_LIST)
		do
			sublist := Void
		end

	split (n: INTEGER_32)
			-- Do nothing.
		require -- from TWO_WAY_LIST
			not_off: not child_off
			valid_sublist: n >= 0
		do
		end

	wipe_out
			-- Remove all items.
			-- (from TWO_WAY_LIST)
		require -- from COLLECTION
			prunable: prunable
		do
			ll_wipe_out
			last_child := Void
		ensure -- from COLLECTION
			wiped_out: is_leaf
--		ensure then -- from DYNAMIC_LIST
			is_before: child_before
--		ensure -- from TREE
			is_leaf: is_leaf
		end

feature {NONE} -- Removal

	ll_wipe_out
			-- Remove all items.
			-- (from LINKED_LIST)
		require -- from COLLECTION
			prunable: prunable
		do
			internal_wipe_out
		ensure -- from COLLECTION
			wiped_out: is_leaf
--		ensure then -- from DYNAMIC_LIST
			is_before: child_before
		end

	chain_wipe_out
			-- Remove all items.
			-- (from DYNAMIC_CHAIN)
		require -- from COLLECTION
			prunable: prunable
		do
			from
				child_start
			until
				is_leaf
			loop
				remove_child
			end
		ensure -- from COLLECTION
			wiped_out: is_leaf
		end

feature -- Transformation

	swap (i: INTEGER_32)
			-- Exchange item at `i'-th position with item
			-- at cursor position.
			-- (from CHAIN)
		require -- from CHAIN
			not_off: not child_off
			valid_index: valid_index (i)
		local
			old_item, new_item: like child_item
			pos: FLAT_CURSOR
		do
			pos := child_cursor
			old_item := child_item
			child_go_i_th (i)
			new_item := child_item
			child_replace (old_item)
			child_go_to (pos)
			child_replace (new_item)
		ensure -- from CHAIN
			swapped_to_item: child_item = old i_th (i)
			swapped_from_item: i_th (i) = old child_item
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

	fill_from_binary (b: FLAT_BINARY_TREE [G])
			-- Fill from a binary tree representation.
			-- Left child becomes first child.
			-- Right child becomes right sibling.
			-- Any right child of `b' is ignored.
			-- (from DYNAMIC_TREE)
		local
			current_node: FLAT_BINARY_TREE [G]
			c: like child
		do
			replace (b.item)
			wipe_out
			if b.has_left then
				from
					current_node := b.left_child
				until
					current_node = Void
				loop
					child_put_right (current_node.item)
					child_forth
					c := child
					if c /= Void then
						c.fill_from_binary (current_node)
					end
					current_node := current_node.right_child
				end
			end
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

	copy (other: FLAT_TWO_WAY_TREE [G])
			-- Copy contents from `other'.
			-- (from TREE)
		local
			i: INTEGER_32
			old_idx: INTEGER_32
			tmp_tree: FLAT_TWO_WAY_TREE [G]
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
				i > child_capacity
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

	duplicate (n: INTEGER_32): FLAT_TWO_WAY_TREE [G]
			-- Copy of sub-tree beginning at cursor position and
			-- having min (`n', arity - child_index + 1)
			-- children
			-- (from DYNAMIC_TREE)
		require -- from TREE
			not_child_off: not child_off
			valid_sublist: n >= 0
		local
			pos: FLAT_CURSOR
			counter: INTEGER_32
			c: like child
		do
			from
				Result := new_tree
				pos := child_cursor
			until
				child_after or else (counter = n)
			loop
				c := child
				if c /= Void then
					Result.put_child (c.duplicate_all)
				end
				child_forth
				counter := counter + 1
			end
			child_go_to (pos)
		end

feature {NONE} -- Duplication

	twl_duplicate (n: INTEGER_32): FLAT_TWO_WAY_TREE [G]
			-- Copy of sub-chain beginning at current position
			-- and having min (`n', `from_here') items,
			-- where `from_here' is the number of items
			-- at or to the right of current position.
			-- (from DYNAMIC_CHAIN)
		require -- from CHAIN
			not_off_unless_after: child_off implies child_after
			valid_subchain: n >= 0
		local
			pos: FLAT_CURSOR
			counter: INTEGER_32
		do
			from
				Result := new_chain
				if object_comparison then
					Result.compare_objects
				end
				pos := child_cursor
			until
				(counter = n) or else exhausted
			loop
				Result.child_extend (child_item)
				child_forth
				counter := counter + 1
			end
			child_go_to (pos)
		end

feature {FLAT_TWO_WAY_TREE} -- Implementation

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
			arity := 0
			child := Void
			child_after := False
			child_before := True
			first_child := Void
			last_child := Void
			left_sibling := Void
			parent := Void
			right_sibling := Void
			sublist := Void
		ensure -- from TREE
			object_comparison_copied: object_comparison = n.object_comparison
			same_arity: arity = old arity
			same_item: item = old item
			result_is_root: is_root
			result_is_leaf: is_leaf
		end

	new_cell (v: like item): like Current
			-- New cell containing `v'
		require -- from  LINKED_LIST
			True
		do
			create Result.make (v)
			if object_comparison then
				Result.compare_objects
			end
			Result.attach_to_parent (Current)
		ensure -- from LINKED_LIST
			result_exists: Result /= Void
		end

	new_chain: like Current
			-- A newly created instance of the same type.
			-- This feature may be redefined in descendants so as to
			-- produce an adequately allocated and initialized object.
		require -- from  DYNAMIC_CHAIN
			True
		do
			Result := new_tree
		ensure -- from DYNAMIC_CHAIN
			result_exists: Result /= Void
		end

	new_tree: like Current
			-- A newly created instance of the same type, with
			-- the same node value.
			-- This feature may be redefined in descendants so as to
			-- produce an adequately allocated and initialized object.
		do
			create Result.make (item)
		ensure -- from DYNAMIC_TREE
			result_exists: Result /= Void
			result_item: Result.item = item
		end

feature {NONE} -- Implementation

	attach (other: like new_cell)
			-- Attach all children of `other' to current node.
		local
			cursor: like child_cursor
			c: like child
		do
			cursor := other.child_cursor
			from
				other.child_start
			until
				other.child_off
			loop
				c := other.child
				if c /= Void then
					c.attach_to_parent (Current)
				end
				other.child_forth
			end
			other.child_go_to (cursor)
		end

	child_remove
			-- Remove item of current child
			-- (from TREE)
		do
		end

	frozen internal_wipe_out
			-- Remove all items.
			-- (from LINKED_LIST)
		require -- from LINKED_LIST
			prunable
		do
			child := Void
			first_child := Void
			child_before := True
			child_after := False
			arity := 0
		ensure -- from LINKED_LIST
			wiped_out: is_leaf
			is_before: child_before
		end

	remove
			-- Remove current item
			-- (from TREE)
		do
		end

	tree_copy (other, tmp_tree: FLAT_TWO_WAY_TREE [G])
			-- Generic implementation of copy. `other' is copied onto
			-- `Current'. `tmp_tree' is used as temporary storage during
			-- copying. Since it cannot be created locally because of the
			-- generic implementation, it has to be passed in.
			-- (from TREE)
		require -- from TREE
			other_not_empty: other /= Void and then not other.is_leaf
			other_not_leaf: not other.is_leaf
			tmp_tree_exists: tmp_tree /= Void
			same_rule: object_comparison = other.object_comparison
		local
			i: INTEGER_32
			p1, p2, node: FLAT_TWO_WAY_TREE [G]
			c1: like child
			other_stack, tmp_stack: FLAT_LINKED_STACK [FLAT_TWO_WAY_TREE [G]]
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
				i > p1.child_capacity and other_stack.is_empty
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
				if i <= p1.child_capacity then
					i := i + 1
				else
					from
					invariant
						same_count: other_stack.count = tmp_stack.count and tmp_stack.count = idx_stack.count
					until
						other_stack.is_empty or else i <= p1.child_capacity
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

	tree_is_equal (t1, t2: FLAT_TWO_WAY_TREE [G]): BOOLEAN
			-- Are `t1' and `t2' recursively equal?
			-- (from TREE)
		require -- from TREE
			trees_exist: t1 /= Void and t2 /= Void
			trees_not_empty: not t1.is_leaf and not t2.is_leaf
			same_rule: t1.object_comparison = t2.object_comparison
		local
			p1, p2: FLAT_TWO_WAY_TREE [G]
			c1, c2: like child
			t1_stack, t2_stack: FLAT_LINKED_STACK [FLAT_TWO_WAY_TREE [G]]
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
					if p1.child_readable and p2.child_readable and p1.child_capacity = p2.child_capacity then
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
					elseif p1.child_capacity /= p2.child_capacity or else (p1.child_readable xor p2.child_readable) then
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

feature {FLAT_TWO_WAY_TREE} -- Implementation

	duplicate_all: FLAT_TWO_WAY_TREE [G]
			-- Copy of sub-tree including all children
			-- (from DYNAMIC_TREE)
		local
			pos: FLAT_CURSOR
			c: like child
		do
			from
				Result := new_tree
				pos := child_cursor
				child_start
			until
				child_off
			loop
				c := child
				if c /= Void then
					Result.put_child (c.duplicate_all)
				end
				Result.child_forth
				child_forth
			end
			child_go_to (pos)
		end

	fill_subtree (other: FLAT_TWO_WAY_TREE [G])
			-- Fill children with children of `other'.
			-- (from DYNAMIC_TREE)
		local
			c: like child
			o: FLAT_TWO_WAY_TREE [G]
		do
			from
				other.child_start
			until
				other.child_off
			loop
				child_extend (other.item)
				other.child_forth
			end
			from
				child_start
				other.child_start
			until
				child_off
			loop
				c := child
				o := other.child
				if c /= Void and then o /= Void then
					c.fill_subtree (o)
				end
				other.child_forth
				child_forth
			end
		end

feature {FLAT_TWO_WAY_TREE} -- Implementation

	attach_to_parent (n: like parent)
			-- Make `n' parent of current node.
			-- (from TREE)
		do
			parent := n
		ensure -- from TREE
			new_parent: parent = n
		end

	fill_list (al: FLAT_ARRAYED_LIST [G])
			-- Fill `al' with all the children's items.
			-- (from TREE)
		local
			c: like child
		do
			from
				child_start
			until
				child_off
			loop
				c := child
				if c /= Void then
					al.extend (child_item)
					c.fill_list (al)
				end
				child_forth
			end
		end

	subtree_count: INTEGER_32
			-- Number of items in children
			-- (from TREE)
		local
			pos: FLAT_CURSOR
			c: like child
		do
			Result := arity
			from
				pos := child_cursor
				child_start
			until
				child_off
			loop
				c := child
				if c /= Void then
					Result := Result + c.subtree_count
				end
				child_forth
			end
			child_go_to (pos)
		end

	subtree_has (v: G): BOOLEAN
			-- Do children include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from TREE)
		local
			cursor: FLAT_CURSOR
			c: like child
		do
			cursor := child_cursor
			from
				child_start
			until
				child_off or else Result
			loop
				if child /= Void then
					if object_comparison then
						Result := v ~ child_item
					else
						Result := v = child_item
					end
				end
				child_forth
			end
			from
				child_start
			until
				child_off or else Result
			loop
				c := child
				if c /= Void then
					Result := c.subtree_has (v)
				end
				child_forth
			end
			child_go_to (cursor)
		end

feature -- Implementation

	child: like first_child
			-- Element at cursor position
			-- (from LINKED_LIST)

feature {FLAT_TWO_WAY_TREE} -- Implementation

	make_sublist (first_item, last_item: like first_child; n: INTEGER_32)
			-- Create sublist
			-- (from TWO_WAY_LIST)
		do
			twl_make
			first_child := first_item
			last_child := last_item
			child := first_child
			arity := n
		end

	previous: like first_child
			-- Element left of cursor
			-- (from TWO_WAY_LIST)
		local
			a: like child
		do
			if child_after then
				Result := child
			else
				a := child
				if a /= Void then
					Result := a.left_sibling
				end
			end
		end

feature {FLAT_TWO_WAY_TREE} -- Implementation

	cleanup_after_remove (v: like first_child)
			-- Clean-up a just removed cell.
			-- (from LINKED_LIST)
		require -- from LINKED_LIST
			non_void_cell: v /= Void
		do
		end

	next: like first_child
			-- Element right of cursor
			-- (from LINKED_LIST)
		local
			a: like child
		do
			if child_before then
				Result := child
			else
				a := child
				if a /= Void then
					Result := a.right_sibling
				end
			end
		end

feature -- Iteration

	do_all (action: PROCEDURE [ANY, TUPLE [G]])
			-- Apply `action' to every item.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		require -- from TRAVERSABLE
			action_exists: action /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_TWO_WAY_TREE [G]
		do
			if attached {FLAT_TWO_WAY_TREE [G]} Current as acs then
				cs := acs
				c := acs.child_cursor
			end
			from
				child_start
			until
				child_after
			loop
				action.call ([child_item])
				child_forth
			end
			if cs /= Void and c /= Void then
				cs.child_go_to (c)
			end
		end

	do_if (action: PROCEDURE [ANY, TUPLE [G]]; test: FUNCTION [ANY, TUPLE [G], BOOLEAN])
			-- Apply `action' to every item that satisfies `test'.
			-- Semantics not guaranteed if `action' or `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		require -- from TRAVERSABLE
			action_exists: action /= Void
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_TWO_WAY_TREE [G]
		do
			if attached {FLAT_TWO_WAY_TREE [G]} Current as acs then
				cs := acs
				c := acs.child_cursor
			end
			from
				child_start
			until
				child_after
			loop
				if test.item ([child_item]) then
					action.call ([child_item])
				end
				child_forth
			end
			if cs /= Void and c /= Void then
				cs.child_go_to (c)
			end
		end

	for_all (test: FUNCTION [ANY, TUPLE [G], BOOLEAN]): BOOLEAN
			-- Is `test' true for all items?
			-- Semantics not guaranteed if `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		require -- from TRAVERSABLE
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_TWO_WAY_TREE [G]
		do
			if attached {FLAT_TWO_WAY_TREE [G]} Current as acs then
				cs := acs
				c := acs.child_cursor
			end
			from
				child_start
				Result := True
			until
				child_after or not Result
			loop
				Result := test.item ([child_item])
				child_forth
			end
			if cs /= Void and c /= Void then
				cs.child_go_to (c)
			end
		ensure then -- from LINEAR
			empty: is_leaf implies Result
		end

	there_exists (test: FUNCTION [ANY, TUPLE [G], BOOLEAN]): BOOLEAN
			-- Is `test' true for at least one item?
			-- Semantics not guaranteed if `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		require -- from TRAVERSABLE
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_TWO_WAY_TREE [G]
		do
			if attached {FLAT_TWO_WAY_TREE [G]} Current as acs then
				cs := acs
				c := acs.child_cursor
			end
			from
				child_start
			until
				child_after or Result
			loop
				Result := test.item ([child_item])
				child_forth
			end
			if cs /= Void and c /= Void then
				cs.child_go_to (c)
			end
		end

invariant
	off_constraint: (child = Void) implies child_off

		-- from DYNAMIC_TREE
	extendible_definition: Extendible
	child_after_definition: child_after = (child_index = arity + 1)

		-- from TREE
	tree_consistency: child_readable implies (attached child as c and then c.parent = Current)
	leaf_definition: is_leaf = (arity = 0)
	child_off_definition: child_off = child_before or child_after
	child_before_definition: child_before = (child_index = 0)
	child_isfirst_definition: child_isfirst = (not is_leaf and child_index = 1)
	child_islast_definition: child_islast = (not is_leaf and child_index = child_capacity)
	child_after_definition: child_after = (child_index >= child_capacity + 1)

		-- from BI_LINKABLE
	right_symmetry: attached right_sibling as r implies (r.left_sibling = Current)
	left_symmetry: attached left_sibling as l implies (l.right_sibling = Current)

		-- from TWO_WAY_LIST
	non_empty_list_has_two_endpoints: not is_leaf implies (first_child /= Void and last_child /= Void)
	empty_list_has_no_endpoints: is_leaf implies last_child = Void
	first_element_constraint: attached first_child as f implies f.left_sibling = Void
	last_element_constraint: attached last_child as l implies l.right_sibling = Void

		-- from LINKED_LIST
	prunable: prunable
	empty_constraint: is_leaf implies ((first_child = Void) and (child = Void))
	not_void_unless_empty: (child = Void) implies is_leaf
	before_constraint: child_before implies (child = first_child)
	after_constraint: child_after implies (child = last_child)

		-- from LIST
	before_definition: child_before = (child_index = 0)
	after_definition: child_after = (child_index = arity + 1)

		-- from CHAIN
	non_negative_index: child_index >= 0
	index_small_enough: child_index <= arity + 1
	off_definition: child_off = ((child_index = 0) or (child_index = arity + 1))
	isfirst_definition: child_isfirst = ((not is_leaf) and (child_index = 1))
	islast_definition: child_islast = ((not is_leaf) and (child_index = arity))
	item_corresponds_to_index: (not child_off) implies (child_item = i_th (child_index))
	index_set_has_same_count: index_set.count = arity

		-- from ACTIVE
	writable_constraint: child_writable implies child_readable
	empty_constraint: is_leaf implies (not child_readable) and (not child_writable)

		-- from BILINEAR
	not_both: not (child_after and child_before)
	before_constraint: child_before implies child_off

		-- from LINEAR
	after_constraint: child_after implies child_off

		-- from TRAVERSABLE
	empty_constraint: is_leaf implies child_off

		-- from FINITE
	empty_definition: is_leaf = (arity = 0)

end -- class TWO_WAY_TREE

