note
	model: sequence, index, object_comparison, sublist

class
	FLAT_TWO_WAY_SORTED_SET [G -> COMPARABLE]

inherit
	FLAT_LINEAR_SUBSET [G]
		redefine
			copy,
			is_equal,
			fill,
			merge,
			prune_all,
			intersect,
			subtract
		end

	FLAT_READABLE_INDEXABLE [G]
		rename
			item as i_th alias "[]"
		redefine
			copy,
			is_equal,
			new_cursor
		end

	V_ORDER [G]
		undefine
			copy,
			is_equal
		end

create
	make


create {FLAT_TWO_WAY_SORTED_SET}
	make_sublist

feature {NONE} -- Initialization

	make
			-- Create an empty list.
			-- (from LINKED_LIST)
		do
			before := True
		ensure
			sequence_effect: sequence.is_empty
			index_effect: index = 0
			object_comparison_effect: not object_comparison
			sublist_effect: sublist = Void
		end

feature -- Access

	at alias "@" (i: INTEGER_32): like item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in CHAIN as synonym of i_th.
			-- (from CHAIN)
		require
			valid_key: valid_index (i)
		local
			pos: FLAT_CURSOR
		do
			pos := cursor
			go_i_th (i)
			Result := item
			go_to (pos)
		ensure
			definition: Result = sequence [i]
		end

	cursor: FLAT_TWO_WAY_LIST_CURSOR [G]
			-- Current cursor position
			-- (from TWO_WAY_LIST)
		do
			create Result.make (active, after, before)
		end

	first: like item
			-- Item at first position
			-- (from LINKED_LIST)
		require
			not_empty: not is_empty
		local
			f: like first_element
		do
			f := first_element
			check
				f_attached: f /= Void
			end
			Result := f.item
		ensure
			definition: Result = sequence.first
		end

	has (v: G): BOOLEAN
			-- Does structure include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from PART_SORTED_LIST)
		local
			pos: FLAT_CURSOR
		do
			if not is_empty then
				pos := cursor
				start
				search (v)
				Result := not after
				go_to (pos)
			end
		end

	i_th alias "[]" (i: INTEGER_32): like item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in CHAIN as synonym of at.
			-- (from CHAIN)
		local
			pos: FLAT_CURSOR
		do
			pos := cursor
			go_i_th (i)
			Result := item
			go_to (pos)
		end

	index: INTEGER_32
			-- Index of current position
			-- (from LINKED_LIST)
		local
			l_active, l_active_iterator: like active
		do
			if after then
				Result := count + 1
			elseif not before then
				from
					Result := 1
					l_active := active
					l_active_iterator := first_element
				until
					l_active_iterator = l_active or else l_active_iterator = Void
				loop
					l_active_iterator := l_active_iterator.right
					Result := Result + 1
				end
			end
		end

	index_of (v: like item; i: INTEGER_32): INTEGER_32
			-- Index of `i'-th occurrence of item identical to `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- 0 if none.
			-- (from CHAIN)
		require
			positive_occurrences: i > 0
		local
			pos: FLAT_CURSOR
		do
			pos := cursor
			Result := sequential_index_of (v, i)
			go_to (pos)
		ensure
			definition_has: set_has (set, v) and i = 1 implies Result = index_that (sequence, agent element_equal (v, ?))
			definition_not_has: not set_has (set, v) or i > 1 implies Result = 0
		end

	item: G
			-- Current item
			-- (from LINKED_LIST)
		local
			a: like active
		do
			a := active
			check
				a_attached: a /= Void
			end
			Result := a.item
		end

	item_for_iteration: G
			-- Item at current position
			-- (from LINEAR)
		require
			not_off: not off
		do
			Result := item
		ensure
			definition: Result = sequence [index]
		end

	last: like item
			-- Item at last position
			-- (from LINKED_LIST)
		require
			not_empty: not is_empty
		local
			l: like last_element
		do
			l := last_element
			check
				l_attached: l /= Void
			end
			Result := l.item
		ensure
			definition: Result = sequence.last
		end

	new_cursor: FLAT_TWO_WAY_SORTED_SET_ITERATION_CURSOR [G]
			-- Fresh cursor associated with current structure
			-- (from TWO_WAY_LIST)
		do
			create Result.make (Current)
			Result.start
		end

	search_after (v: like item)
			-- Go to first position with item greater
			-- than or equal to `v'.
			-- (from PART_SORTED_LIST)
		note
			modify: index
		do
			from
				start
			until
				after or else v <= item
			loop
				forth
			end
		ensure
			index_effect: sequence.front (index - 1).range.for_all (agent less (?, v)) and
				sequence.tail (index).range.for_all (agent greater_equal (?, v))
		end

	search_before (v: like item)
			-- Go to last position with item less
			-- than or equal to `v'.
			-- (from PART_SORTED_LIST)
		note
			modify: index
		do
			from
				finish
			until
				before or else v >= item
			loop
				back
			end
		ensure
			index_effect: sequence.front (index).range.for_all (agent less_equal (?, v)) and
				sequence.tail (index + 1).range.for_all (agent greater (?, v))
		end

	sublist: FLAT_TWO_WAY_SORTED_SET [G]
			-- Result produced by last split
			-- (from TWO_WAY_LIST)

feature {FLAT_TWO_WAY_SORTED_SET, FLAT_TWO_WAY_SORTED_SET_ITERATION_CURSOR} -- Access

	first_element: like new_cell
			-- Head of list
			-- (from LINKED_LIST)

	last_element: like first_element
			-- Tail of the list
			-- (from TWO_WAY_LIST)						

feature {NONE} -- Access

	sequential_has (v: like item): BOOLEAN
			-- Does structure include an occurrence of `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		do
			start
			if not off then
				search (v)
			end
			Result := not exhausted
		end

	sequential_index_of (v: like item; i: INTEGER_32): INTEGER_32
			-- Index of `i'-th occurrence of `v'.
			-- 0 if none.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		require
			positive_occurrences: i > 0
		local
			occur, pos: INTEGER_32
		do
			if object_comparison and v /= Void then
				from
					start
					pos := 1
				until
					exhausted or (occur = i)
				loop
					if item ~ v then
						occur := occur + 1
					end
					forth
					pos := pos + 1
				end
			else
				from
					start
					pos := 1
				until
					exhausted or (occur = i)
				loop
					if item = v then
						occur := occur + 1
					end
					forth
					pos := pos + 1
				end
			end
			if occur = i then
				Result := pos - 1
			end
		end

	sequential_occurrences (v: like item): INTEGER_32
			-- Number of times `v' appears.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		do
			from
				start
				search (v)
			until
				exhausted
			loop
				Result := Result + 1
				forth
				search (v)
			end
		end

	sequential_search (v: like item)
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
					exhausted or else v ~ item
				loop
					forth
				end
			else
				from
				until
					exhausted or else v = item
				loop
					forth
				end
			end
		end

feature {NONE} -- Measurement

	cs_max: like item
			-- Maximum item
			-- (from COMPARABLE_STRUCT)
		require
			min_max_available
		do
			from
				start
				Result := item
				forth
			until
				off
			loop
				if item > Result then
					Result := item
				end
				forth
			end
		end

	cs_min: like item
			-- Minimum item
			-- (from COMPARABLE_STRUCT)
		require
			min_max_available
		do
			from
				start
				Result := item
				forth
			until
				off
			loop
				if item < Result then
					Result := item
				end
				forth
			end
		end

	min_max_available: BOOLEAN
			-- Can min and max be computed?
			-- (from COMPARABLE_STRUCT)
		do
			Result := not is_empty
		end

feature -- Measurement

	count: INTEGER_32
			-- Number of items
			-- (from LINKED_LIST)

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
			-- (from CHAIN)
		do
			create Result.make (1, count)
		end

	max: like item
			-- Maximum item
			-- (from SORTED_LIST)
		require
			not_empty: not is_empty
		do
			Result := last
		ensure
			definition: Result = set.extremum (agent greater_equal)
		end

	median: like item
			-- Median item
			-- (from SORTED_LIST)
		require
			not_empty: not is_empty
		do
			Result := i_th ((count + 1) // 2)
		ensure
			definition: set [Result] and
				((set | agent less (?, Result)).count - (set | agent greater (?, Result)).count).abs <= 1
		end

	min: like item
			-- Minimum item
			-- (from SORTED_LIST)
		require
			not_empty: not is_empty
		do
			Result := first
		ensure
			definition: Result = set.extremum (agent less_equal)
		end

	occurrences (v: like item): INTEGER_32
			-- Number of times `v' appears.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from CHAIN)
		local
			pos: FLAT_CURSOR
		do
			pos := cursor
			Result := sequential_occurrences (v)
			go_to (pos)
		ensure
			definition_has: set_has (set, v) implies Result = 1
			definition_not_has: not set_has (set, v) implies Result = 0
		end

feature -- Comparison

	is_equal (other: FLAT_TWO_WAY_SORTED_SET [G]): BOOLEAN
			-- Does `other' contain the same elements?
			-- (from LIST)
		do
			if Current = other then
				Result := True
			else
				Result := (is_empty = other.is_empty) and (object_comparison = other.object_comparison) and (count = other.count)
				if Result and not is_empty then
					if attached {FLAT_CURSOR} cursor as c1 and then attached {FLAT_CURSOR} other.cursor as c2 then
						from
							start
							other.start
						until
							after or not Result
						loop
							if object_comparison then
								Result := item ~ other.item
							else
								Result := item = other.item
							end
							forth
							other.forth
						end
						go_to (c1)
						other.go_to (c2)
					else
						check
							cursors_exist: False
						end
					end
				elseif is_empty and other.is_empty and object_comparison = other.object_comparison then
					Result := True
				end
			end
		ensure then
			definition: Result = (object_comparison = other.object_comparison and
				set.count = other.set.count and
				other.set.for_all (agent (x: G): BOOLEAN
					do
						Result := set_has (set, x)
					end))
		end

feature -- Status report

	after: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?
			-- (from LINKED_LIST)

	before: BOOLEAN
			-- Is there no valid cursor position to the left of cursor?
			-- (from LINKED_LIST)

	exhausted: BOOLEAN
			-- Has structure been completely explored?
			-- (from LINEAR)
		do
			Result := off
		ensure
			definition: Result = not sequence.domain [index]
		end

	extendible: BOOLEAN
			-- May new items be added? (Answer: yes.)
			-- (from DYNAMIC_CHAIN)
		do
			Result := True
		end

	Full: BOOLEAN = False
			-- Is structured filled to capacity? (Answer: no.)
			-- (from LINKED_LIST)

	is_empty: BOOLEAN
			-- Is structure empty?
			-- (from FINITE)
		do
			Result := (count = 0)
		end

	isfirst: BOOLEAN
			-- Is cursor at first position?
			-- (from LINKED_LIST)
		do
			Result := not after and not before and (active = first_element)
		ensure
			definition: Result = (not sequence.is_empty and index = 1)
		end

	islast: BOOLEAN
			-- Is cursor at last position?
			-- (from TWO_WAY_LIST)
		do
			Result := (active = last_element) and then not after and then not before
		end

	off: BOOLEAN
			-- Is there no current item?
			-- (from LINKED_LIST)
		do
			Result := after or before
		end

	prunable: BOOLEAN
			-- May items be removed? (Answer: yes.)
			-- (from DYNAMIC_CHAIN)
		do
			Result := True
		end

	readable: BOOLEAN
			-- Is there a current item that may be read?
			-- (from LINKED_LIST)
		do
			Result := not off
		ensure
			definition: Result = sequence.domain [index]
		end

	replaceable: BOOLEAN
			-- Can current item be replaced?
			-- (from ACTIVE)
		do
			Result := True
		ensure
			definition: Result
		end

	sorted: BOOLEAN
			-- Is the structure sorted?
			-- (from SORTED_TWO_WAY_LIST)
		local
			c: FLAT_CURSOR
			prev: like item
		do
			Result := True
			if count > 1 then
				from
					c := cursor
					start
					check
						not off
					end
					prev := item
					forth
				until
					after or not Result
				loop
					Result := (prev <= item)
					prev := item
					forth
				end
				go_to (c)
			end
		ensure
			definition: Result = sequence.but_last.domain.for_all (agent (i: INTEGER): BOOLEAN
				do
					Result := sequence [i] < sequence [i + 1]
				end)
		end

	valid_cursor (p: FLAT_CURSOR): BOOLEAN
			-- Can the cursor be moved to position `p'?
			-- (from LINKED_LIST)
		local
			temp, sought: like first_element
		do
			if attached {like cursor} p as ll_c then
				from
					temp := first_element
					sought ?= ll_c.active
					Result := ll_c.after or else ll_c.before
				until
					Result or else temp = Void
				loop
					Result := (temp = sought)
					temp := temp.right
				end
			end
		end

	valid_cursor_index (i: INTEGER_32): BOOLEAN
			-- Is `i' correctly bounded for cursor movement?
			-- (from CHAIN)
		do
			Result := (i >= 0) and (i <= count + 1)
		ensure
			definition: Result = (0 <= i and i <= sequence.count + 1)
		end

	valid_index (i: INTEGER_32): BOOLEAN
			-- Is `i' within allowable bounds?
			-- (from CHAIN)
		do
			Result := (i >= 1) and (i <= count)
		end

	writable: BOOLEAN
			-- Is there a current item that may be modified?
			-- (from SEQUENCE)
		do
			Result := not off
		ensure
			definition: Result = sequence.domain [index]
		end

feature -- Cursor movement

	back
			-- Move cursor to previous position, if any.
			-- (from TWO_WAY_LIST)
		note
			modify: index
		require -- from BILINEAR
			not_before: not before
		local
			a: like active
		do
			if after then
				after := False
				if is_empty then
					before := True
				end
			else
				a := active
				if a /= Void then
					a := a.left
					if a = Void then
						active := first_element
						before := True
					else
						active := a
					end
				end
			end
		ensure
			index_effect: index = old index - 1
		end

	finish
			-- Move cursor to last position.
			-- (Go before if empty)
			-- (from TWO_WAY_LIST)
		note
			modify: index
		do
			if not is_empty then
				active := last_element
				after := False
				before := False
			else
				after := False
				before := True
			end
		ensure
			index_effect: index = sequence.count
		end

	forth
			-- Move cursor to next position, if any.
			-- (from TWO_WAY_LIST)
		local
			a: like active
		do
			if before then
				before := False
				if is_empty then
					after := True
				end
			else
				a := active
				if a /= Void then
					a := a.right
					if a = Void then
						active := last_element
						after := True
					else
						active := a
					end
				end
			end
		end

	go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th position.
			-- (from LINKED_LIST)
		require else
			valid_cursor_index: valid_cursor_index (i)
		do
			if i = 0 then
				before := True
				after := False
				active := first_element
			elseif i = count + 1 then
				before := False
				after := True
				active := last_element
			else
				move (i - index)
			end
		end

	go_to (p: FLAT_CURSOR)
			-- Move cursor to position `p'.
			-- (from LINKED_LIST)
		note
			modify: index
		require
			cursor_position_valid: valid_cursor (p)
		do
			if attached {like cursor} p as ll_c then
				after := ll_c.after
				before := ll_c.before
				if before then
					active := first_element
				elseif after then
					active := last_element
				else
					active ?= ll_c.active
				end
			else
				check
					correct_cursor_type: False
				end
			end
		end

	move (i: INTEGER_32)
			-- Move cursor `i' positions. The cursor
			-- may end up off if the offset is to big.
			-- (from TWO_WAY_LIST)
		note
			modify: index
		local
			counter: INTEGER_32
			p: like first_element
		do
			if i > 0 then
				ll_move (i)
			elseif i < 0 then
				if after then
					after := False
					counter := -1
				end
				from
					p := active
				until
					(counter = i) or else (p = Void)
				loop
					p := p.left
					counter := counter - 1
				end
				if p = Void then
					before := True
					active := first_element
				else
					active := p
				end
			end
		ensure
			index_effect_too_far_right: (old index + i > sequence.count) implies index = sequence.count + 1
			index_effect_too_far_left: (old index + i < 1) implies index = 0
			index_effect_expected: sequence.domain [old index + i] implies index = old index + i
		end

	search (v: like item)
			-- Move to first position (at or after current
			-- position) where item and `v' are equal.
			-- If structure does not include `v' ensure that
			-- exhausted will be true.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from BILINEAR)
		note
			modify: index
		do
			if before and not is_empty then
				forth
			end
			sequential_search (v)
		ensure
			index_effect_has: sequence.tail (old index).range.exists (agent element_equal (v, ?)) implies
				element_equal (v, sequence [index]) and not sequence.interval (old index, index - 1).range.exists (agent element_equal (v, ?))
			index_effect_not_has: not sequence.tail (old index).range.exists (agent element_equal (v, ?)) implies
				index = sequence.count + 1
		end

	start
			-- Move cursor to first position.
			-- (from LINKED_LIST)
		do
			if first_element /= Void then
				active := first_element
				after := False
			else
				after := True
			end
			before := False
		end

feature {NONE} -- Cursor movement

	ll_move (i: INTEGER_32)
			-- Move cursor `i' positions. The cursor
			-- may end up off if the offset is too big.
			-- (from LINKED_LIST)
		local
			counter, new_index: INTEGER_32
			p: like first_element
		do
			if i > 0 then
				if before then
					before := False
					counter := 1
				end
				from
					p := active
				until
					(counter = i) or else (p = Void)
				loop
					active := p
					p := p.right
					counter := counter + 1
				end
				if p = Void then
					after := True
				else
					active := p
				end
			elseif i < 0 then
				new_index := index + i
				before := True
				after := False
				active := first_element
				if (new_index > 0) then
					move (new_index)
				end
			end
		end

feature -- Element change

	append (s: FLAT_TWO_WAY_SORTED_SET [G])
			-- Append a copy of `s'.
			-- (from CHAIN)
		note
			modify: sequence
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
				l_cursor := cursor
				l.start
			until
				l.exhausted
			loop
				extend (l.item)
				finish
				l.forth
			end
			go_to (l_cursor)
		ensure
			set_effect_old: (old set).for_all (agent set_has (set, ?))
			set_effect_other: (old s.set).for_all (agent set_has (set, ?))
			set_effect_new: set.for_all (agent (x: G; c, o: MML_SET [G]): BOOLEAN
				do
					Result := set_has (c, x) or set_has (o, x)
				end (?, old set, old s.set))
		end

	extend (v: G)
			-- Ensure that structure includes `v'.
			-- Was declared in TWO_WAY_SORTED_SET as synonym of put.
		local
			found: BOOLEAN
			b: BOOLEAN
		do
			search_after (v)
			b := after
			if not b then
				b := item /~ v
			end
			if b then
				put_left (v)
				back
			end
			if object_comparison then
				if after or else item /~ v then
					put_left (v)
					back
				end
			else
				from
				until
					found or after or else item /~ v
				loop
					found := item = v
					forth
				end
				if not found then
					put_left (v)
				end
				back
			end
		end

	fill (other: FLAT_LINEAR_SUBSET [G])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current structure
			-- need not be the same.
			-- (from CHAIN)
		local
			lin_rep: FLAT_LINEAR_SUBSET [G]
			l_cursor: FLAT_CURSOR
		do
			lin_rep := other.linear_representation
			from
				l_cursor := cursor
				lin_rep.start
			until
				not extendible or else lin_rep.off
			loop
				extend (lin_rep.item)
				finish
				lin_rep.forth
			end
			go_to (l_cursor)
		end

	force (v: like item)
			-- Add `v' to end.
			-- (from SEQUENCE)
		note
			modify: sequence
		require
			extendible: extendible
		do
			extend (v)
		ensure
			set_effect_not_has: not set_has (old set, v) implies set |=| (old set & v)
			set_effect_has: set_has (old set, v) implies set |=| old set
		end

	merge (other: FLAT_LINEAR_SUBSET [G])
			-- Add all items of `other'.
		local
			mode: BOOLEAN
			other_item: like item
			b: BOOLEAN
		do
			from
				mode := object_comparison
				start
				other.start
			until
				after or other.after
			loop
				other_item := other.item
				if item < other_item then
					search_after (other_item)
				end
				if not after then
					b := not mode and then item = other_item
					if not b and then mode then
						b := item ~ other_item
					end
					if b then
						forth
						other.forth
					else
						from
						until
							other.after or else other.item >= item
						loop
							put_left (other.item)
							other.forth
						end
					end
				end
			end
			from
			until
				other.after
			loop
				put_left (other.item)
				other.forth
			end
		end

	merge_left (other: FLAT_TWO_WAY_SORTED_SET [G])
			-- Merge `other' into current structure before cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from TWO_WAY_LIST)
		note
			modify: sequence, index, other_sequence, other_index
		require
			extendible: extendible
			not_before: not before
			other_exists: other /= Void
			not_current: other /= Current
		local
			other_first_element: like first_element
			other_last_element: like first_element
			other_count: INTEGER_32
			a: like active
		do
			if not other.is_empty then
				other_first_element := other.first_element
				other_last_element := other.last_element
				other_count := other.count
				other.wipe_out
				check
					other_first_element /= Void
					other_last_element /= Void
				end
				if is_empty then
					last_element := other_last_element
					first_element := other_first_element
					if before then
						active := first_element
					else
						active := last_element
					end
				elseif isfirst then
					other_last_element.put_right (first_element)
					first_element := other_first_element
				elseif after then
					other_first_element.put_left (last_element)
					last_element := other_last_element
					active := last_element
				else
					a := active
					if a /= Void then
						other_first_element.put_left (a.left)
						a.put_left (other_last_element)
					end
				end
				count := count + other_count
			end
		ensure
			sequence_effect: sequence |=| old (sequence.front (index - 1) + other.sequence + sequence.tail (index))
			index_effect: index = old index + old other.sequence.count
			other_sequence_effect: other.sequence.is_empty
		end

	merge_right (other: FLAT_TWO_WAY_SORTED_SET [G])
			-- Merge `other' into current structure after cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from TWO_WAY_LIST)
		note
			modify: sequence, index, other_sequence, other_index
		require -- from DYNAMIC_CHAIN
			extendible: extendible
			not_after: not after
			other_exists: other /= Void
			not_current: other /= Current
		do
			if is_empty or else islast then
				last_element := other.last_element
			end
			ll_merge_right (other)
		ensure
			sequence_effect: sequence |=| old (sequence.front (index) + other.sequence + sequence.tail (index + 1))
			index_unchanged: index = old index
			other_sequence_effect: other.sequence.is_empty
		end

	put (v: G)
			-- Ensure that structure includes `v'.
			-- Was declared in TWO_WAY_SORTED_SET as synonym of extend.
		local
			found: BOOLEAN
			b: BOOLEAN
		do
			search_after (v)
			b := after
			if not b then
				b := item /~ v
			end
			if b then
				put_left (v)
				back
			end
			if object_comparison then
				if after or else item /~ v then
					put_left (v)
					back
				end
			else
				from
				until
					found or after or else item /~ v
				loop
					found := item = v
					forth
				end
				if not found then
					put_left (v)
				end
				back
			end
		end

	sequence_put (v: like item)
			-- Add `v' to end.
			-- (from SEQUENCE)
		note
			modify: sequence
		require
			extendible: extendible
		do
			extend (v)
		ensure
			set_effect_not_has: not set_has (old set, v) implies set |=| (old set & v)
			set_effect_has: set_has (old set, v) implies set |=| old set
		end

	put_front (v: like item)
			-- Add `v' to beginning.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
		note
			modify: sequence
		require
			extendible: extendible
		do
			ll_put_front (v)
			if count = 1 then
				last_element := first_element
			end
		ensure
			sequence_effect: sequence |=| old sequence.prepended (v)
		end

	put_i_th (v: like item; i: INTEGER_32)
			-- Put `v' at `i'-th position.
			-- (from CHAIN)
		note
			modify: sequence
		require
			valid_key: valid_index (i)
		local
			pos: FLAT_CURSOR
		do
			pos := cursor
			go_i_th (i)
			replace (v)
			go_to (pos)
		ensure
			sequence_effect: sequence |=| old sequence.replaced_at (i, v)
		end

	put_left (v: like item)
			-- Add `v' to the left of cursor position.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
		local
			p: like first_element
			a: like active
		do
			p := new_cell (v)
			if is_empty then
				first_element := p
				last_element := p
				active := p
				before := False
			elseif after then
				p.put_left (last_element)
				last_element := p
				active := p
			elseif isfirst then
				p.put_right (active)
				first_element := p
			else
				a := active
				if a /= Void then
					p.put_left (a.left)
				end
				p.put_right (active)
			end
			count := count + 1
		end

	put_right (v: like item)
			-- Add `v' to the right of cursor position.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
		note
			modify: sequence
		require
			extendible: extendible
			not_after: not after
		local
			was_last: BOOLEAN
			a: like active
		do
			was_last := islast
			ll_put_right (v)
			if count = 1 then
				last_element := active
			elseif was_last then
				a := active
				if a /= Void then
					last_element := a.right
				end
			end
		ensure
			sequence_effect: sequence |=| old sequence.extended_at (index + 1, v)
		end

	replace (v: like item)
			-- Replace current item by `v'.
			-- (from LINKED_LIST)
		note
			modify: sequence
		require
			writable: writable
			replaceable: replaceable
		local
			a: like active
		do
			a := active
			if a /= Void then
				a.put (v)
			end
		ensure
			sequence_effect: sequence |=| old (sequence.replaced_at (index, v))
		end

feature {NONE} -- Element change

	stwl_extend (v: like item)
			-- Put `v' at proper position in list.
			-- Move cursor to newly inserted item.
			-- (from SORTED_TWO_WAY_LIST)
		require -- from COLLECTION
			extendible: extendible
		do
			search_after (v)
			put_left (v)
			back
		end

	stwl_merge (other: FLAT_TWO_WAY_SORTED_SET [G])
			-- Add all items from `other' at their proper positions.
			-- (from PART_SORTED_LIST)
		do
			from
				other.start
			until
				other.off
			loop
				extend (other.item)
				other.forth
			end
		end

	ll_merge_right (other: FLAT_TWO_WAY_SORTED_SET [G])
			-- Merge `other' into current structure after cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: extendible
			not_after: not after
			other_exists: other /= Void
			not_current: other /= Current
		local
			other_first_element: like first_element
			other_last_element: like last_element
			other_count: INTEGER_32
			a: like active
		do
			other_last_element := other.last_element
			if other_last_element /= Void then
				other_first_element := other.first_element
				other_count := other.count
				other.wipe_out
				check
					other_first_element /= Void
					other_last_element /= Void
				end
				a := active
				if a = Void then
					first_element := other_first_element
					active := first_element
				else
					if not islast then
						other_last_element.put_right (a.right)
					end
					a.put_right (other_first_element)
				end
				count := count + other_count
			end
		end

	stwl_put (v: like item)
			-- Replace current item by `v'.
			-- (Synonym for replace)
			-- (from CHAIN)
		require -- from CHAIN
			writeable: writable
			replaceable: replaceable
		do
			replace (v)
		end

	ll_put_front (v: like item)
			-- Add `v' to beginning.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: extendible
		local
			p: like new_cell
		do
			p := new_cell (v)
			p.put_right (first_element)
			first_element := p
			if before or is_empty then
				active := p
			end
			count := count + 1
		end

	ll_put_right (v: like item)
			-- Add `v' to the right of cursor position.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: extendible
			not_after: not after
		local
			p: like new_cell
			a: like active
		do
			p := new_cell (v)
			check
				is_empty implies before
			end
			if before then
				p.put_right (first_element)
				first_element := p
				active := p
			else
				a := active
				if a /= Void then
					p.put_right (a.right)
					a.put_right (p)
				end
			end
			count := count + 1
		end

feature -- Removal

	prune (v: like item)
			-- Remove `v' if present.
		do
			start
			stwl_prune (v)
		end

	prune_all (v: like item)
			-- Remove all items identical to `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- Leave cursor off.
			-- (from SORTED_TWO_WAY_LIST)
		do
			from
				start
				search (v)
			until
				off or else v < item
			loop
				remove
			end
			if not off then
				finish
				forth
			end
		end

	remove
			-- Remove current item.
			-- Move cursor to right neighbor
			-- (or after if no right neighbor).
			-- (from TWO_WAY_LIST)
		local
			succ, pred, removed: like first_element
			a: like active
		do
			removed := active
			if isfirst then
				a := first_element
				if a /= Void then
					active := a.right
					a.forget_right
				end
				first_element := active
				if count = 1 then
					check
						no_active: active = Void
					end
					after := True
					last_element := Void
				end
			elseif islast then
				a := last_element
				if a /= Void then
					active := a.left
					a.forget_left
				end
				last_element := active
				after := True
			else
				a := active
				if a /= Void then
					pred := a.left
					succ := a.right
				end
				if pred /= Void and then succ /= Void then
					pred.forget_right
					succ.forget_left
					pred.put_right (succ)
				end
				active := succ
			end
			count := count - 1
			cleanup_after_remove (removed)
		end

	remove_left
			-- Remove item to the left of cursor position.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
		note
			modify: sequence, index
		require
			left_exists: index > 1
		do
			back
			remove
		ensure
			sequence_effect: sequence |=| old sequence.removed_at (index - 1)
			index_effect: index = old index - 1
		end

	remove_right
			-- Remove item to the right of cursor position.
			-- Do not move cursor.
			-- (from TWO_WAY_LIST)
		note
			modify: sequence
		require
			right_exists: index < count
		do
			forth
			remove
			back
		ensure
			sequence_effect: sequence |=| old sequence.removed_at (index + 1)
		end

	remove_sublist
			-- (from TWO_WAY_LIST)
		note
			modify: sublist
		do
			sublist := Void
		ensure
			sublist_effect: sublist = Void
		end

	split (n: INTEGER_32)
			-- Remove from current list
			-- min (`n', count - index - 1) items
			-- starting at cursor position.
			-- Move cursor right one position.
			-- Make extracted sublist accessible
			-- through attribute sublist.
			-- (from TWO_WAY_LIST)
		note
			modify: sequence, sublist, index
		require
			not_off: not off
			valid_sublist: n >= 0
		local
			actual_number, active_index: INTEGER_32
			p_elem, s_elem, e_elem, n_elem: like first_element
		do
			if n = 0 then
				create sublist.make
			else
				active_index := index
				if active_index + n > count + 1 then
					actual_number := count + 1 - active_index
				else
					actual_number := n
				end
				s_elem := active
				p_elem := previous
				move (actual_number - 1)
				e_elem := active
				n_elem := next
				if s_elem /= Void then
					s_elem.forget_left
				end
				if e_elem /= Void then
					e_elem.forget_right
				end
				create sublist.make_sublist (s_elem, e_elem, actual_number)
				count := count - actual_number
				if p_elem /= Void then
					p_elem.put_right (n_elem)
				else
					first_element := n_elem
				end
				if n_elem /= Void then
					active := n_elem
				else
					last_element := p_elem
					active := p_elem
					after := True
				end
			end
		ensure
			sequence_effect: sequence |=| old sequence.removed ({MML_INTERVAL} [[index, index + n - 1]])
			sublist_sequence_effect: sublist.sequence |=| old sequence.restricted ({MML_INTERVAL} [[index, index + n - 1]])
			sublist_object_comparison_effect: not sublist.object_comparison
			sublist_sublist_effect: sublist.sublist = Void
			index_effect: index = (sequence.count + 1).min (old index)
		end

	wipe_out
			-- Remove all items.
			-- (from TWO_WAY_LIST)
		do
			ll_wipe_out
			last_element := Void
		end

feature {NONE} -- Removal

	stwl_prune (v: like item)
			-- Remove first occurrence of `v', if any,
			-- after cursor position.
			-- If found, move cursor to right neighbor;
			-- if not, make structure exhausted.
			-- (from DYNAMIC_CHAIN)
		require -- from COLLECTION
			prunable: prunable
		do
			search (v)
			if not exhausted then
				remove
			end
		end

	ll_wipe_out
			-- Remove all items.
			-- (from LINKED_LIST)
		require -- from COLLECTION
			prunable: prunable
		do
			internal_wipe_out
		end

	chain_wipe_out
			-- Remove all items.
			-- (from DYNAMIC_CHAIN)
		require -- from COLLECTION
			prunable: prunable
		do
			from
				start
			until
				is_empty
			loop
				remove
			end
		end

feature -- Transformation

	sort
			-- Sort all items.
			-- Has O(count * log (count)) complexity.
			-- (from SORTED_TWO_WAY_LIST)
		note
			modify: sequence
		local
			no_change: BOOLEAN
			gap: INTEGER_32
			left_cell, cell: like first_element
			left_cell_item, cell_item: like item
		do
			if not is_empty then
				from
					gap := count * 10 // 13
				until
					gap = 0
				loop
					from
						no_change := False
						go_i_th (1 + gap)
					until
						no_change
					loop
						no_change := True
						from
							left_cell := first_element
							cell := active
						until
							cell = Void
						loop
							if left_cell /= Void then
								left_cell_item := left_cell.item
								cell_item := cell.item
								if cell_item < left_cell_item then
									no_change := False
									cell.put (left_cell_item)
									left_cell.put (cell_item)
								end
								left_cell := left_cell.right
							end
							cell := cell.right
						end
					end
					gap := gap * 10 // 13
				end
			end
		ensure
			sequence_effect: sequence.but_last.domain.for_all (agent (i: INTEGER): BOOLEAN
				do
					Result := sequence [i] < sequence [i + 1]
				end)
			sequence_constraint: set |=| old set
		end

	swap (i: INTEGER_32)
			-- Exchange item at `i'-th position with item
			-- at cursor position.
			-- (from CHAIN)
		note
			modify: sequence
		require
			not_off: not off
			valid_index: valid_index (i)
		local
			old_item, new_item: like item
			pos: FLAT_CURSOR
		do
			pos := cursor
			old_item := item
			go_i_th (i)
			new_item := item
			replace (old_item)
			go_to (pos)
			replace (new_item)
		ensure
			sequence_effect: sequence |=| old (sequence.replaced_at (i, sequence [index]).replaced_at (index, sequence [i]))
		end

feature -- Conversion

	linear_representation: FLAT_LINEAR_SUBSET [G]
			-- Representation as a linear structure
			-- (from LINEAR)
		do
			Result := Current
		end

feature -- Duplication

	copy (other: FLAT_TWO_WAY_SORTED_SET [G])
			-- Update current object using fields of object attached
			-- to `other', so as to yield equal objects.
			-- (from LINKED_LIST)
		note
			modify: sequence, index, object_comparison, sublist
		local
			cur: FLAT_TWO_WAY_LIST_CURSOR [G]
		do
			if other /= Current then
				standard_copy (other)
				if not other.is_empty then
					internal_wipe_out
					if attached {FLAT_TWO_WAY_LIST_CURSOR [G]} other.cursor as l_cur then
						cur := l_cur
					end
					from
						other.start
					until
						other.off
					loop
						extend (other.item)
						finish
						other.forth
					end
					if cur /= Void then
						other.go_to (cur)
					end
				end
			end
		ensure then
			sequence_effect: sequence |=| other.sequence
			object_comparison_effect: object_comparison = other.object_comparison
			sublist_effect: sublist = other.sublist
		end

	duplicate (n: INTEGER_32): like Current
			-- Copy of sub-set beginning at cursor position
			-- and having min (`n', count - index + 1) items
--		require -- from CHAIN
--			not_off_unless_after: off implies after
--			valid_subchain: n >= 0
		local
			pos: FLAT_CURSOR
			counter: INTEGER_32
		do
			pos := cursor
			Result := new_chain
			Result.finish
			Result.forth
			from
			until
				(counter = n) or else after
			loop
				Result.put_left (item)
				forth
				counter := counter + 1
			end
			go_to (pos)
		end

feature -- Basic operations

	intersect (other: like Current)
			-- Remove all items not in `other'.
		do
			from
				start
				other.start
			until
				after or other.after
			loop
				from
				until
					after or item >= other.item
				loop
					remove
				end
				if not after then
					from
					until
						other.after or else other.item >= item
					loop
						other.forth
					end
					if not other.after then
						if item ~ other.item then
							forth
							other.forth
						end
					end
				end
			end
			if other.after then
				from
				until
					after
				loop
					remove
				end
			end
		end

	subtract (other: like Current)
			-- Remove all items also in `other'.
		local
			other_item: like item
		do
			from
				start
				other.start
			until
				after or other.after
			loop
				other_item := other.item
				if item < other_item then
					search_after (other_item)
				end
				if not after and then item ~ other_item then
					remove
				end
				other.forth
			end
		end

feature {NONE} -- Inapplicable

	subset_symdif (other: FLAT_TWO_WAY_SORTED_SET [G])
			-- Inapplicable symdif.
		require -- from SUBSET
			set_exists: other /= Void
			same_rule: object_comparison = other.object_comparison
		do
		end

feature {NONE} -- Implementation

	frozen internal_wipe_out
			-- Remove all items.
			-- (from LINKED_LIST)
		require -- from LINKED_LIST
			prunable
		do
			active := Void
			first_element := Void
			before := True
			after := False
			count := 0
		ensure -- from LINKED_LIST
			wiped_out: is_empty
			is_before: before
		end

feature {SORTED_TWO_WAY_LIST} -- Implementation

	new_chain: FLAT_TWO_WAY_SORTED_SET [G]
			-- A newly created instance of the same type.
			-- This feature may be redefined in descendants so as to
			-- produce an adequately allocated and initialized object.
			-- (from SORTED_TWO_WAY_LIST)
		require -- from  DYNAMIC_CHAIN
			True
		do
			create Result.make
		ensure -- from DYNAMIC_CHAIN
			result_exists: Result /= Void
		end

feature {FLAT_TWO_WAY_SORTED_SET} -- Implementation

	make_sublist (first_item, last_item: like first_element; n: INTEGER_32)
			-- Create sublist
			-- (from TWO_WAY_LIST)
		do
			make
			first_element := first_item
			last_element := last_item
			active := first_element
			count := n
		end

	new_cell (v: like item): FLAT_BI_LINKABLE [G]
			-- A newly created instance of the type of first_element.
			-- (from TWO_WAY_LIST)
		do
			create Result.put (v)
		ensure -- from LINKED_LIST
			result_exists: Result /= Void
		end

	previous: like first_element
			-- Element left of cursor
			-- (from TWO_WAY_LIST)
		local
			a: like active
		do
			if after then
				Result := active
			else
				a := active
				if a /= Void then
					Result := a.left
				end
			end
		end

feature {FLAT_TWO_WAY_SORTED_SET} -- Implementation

	active: like first_element
			-- Element at cursor position
			-- (from LINKED_LIST)

	cleanup_after_remove (v: like first_element)
			-- Clean-up a just removed cell.
			-- (from LINKED_LIST)
		require -- from LINKED_LIST
			non_void_cell: v /= Void
		do
		end

	next: like first_element
			-- Element right of cursor
			-- (from LINKED_LIST)
		local
			a: like active
		do
			if before then
				Result := active
			else
				a := active
				if a /= Void then
					Result := a.right
				end
			end
		end

feature -- Iteration

	do_all (action: PROCEDURE [ANY, TUPLE [G]])
			-- Apply `action' to every item.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		note
			modify: sequence
		require
			action_exists: action /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_TWO_WAY_SORTED_SET [G]
		do
			if attached {FLAT_TWO_WAY_SORTED_SET [G]} Current as acs then
				cs := acs
				c := acs.cursor
			end
			from
				start
			until
				after
			loop
				action.call ([item])
				forth
			end
			if cs /= Void and c /= Void then
				cs.go_to (c)
			end
		end

	do_if (action: PROCEDURE [ANY, TUPLE [G]]; test: FUNCTION [ANY, TUPLE [G], BOOLEAN])
			-- Apply `action' to every item that satisfies `test'.
			-- Semantics not guaranteed if `action' or `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		note
			modify: sequence
		require
			action_exists: action /= Void
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_TWO_WAY_SORTED_SET [G]
		do
			if attached {FLAT_TWO_WAY_SORTED_SET [G]} Current as acs then
				cs := acs
				c := acs.cursor
			end
			from
				start
			until
				after
			loop
				if test.item ([item]) then
					action.call ([item])
				end
				forth
			end
			if cs /= Void and c /= Void then
				cs.go_to (c)
			end
		end

	for_all (test: PREDICATE [ANY, TUPLE [G]]): BOOLEAN
			-- Is `test' true for all items?
			-- Semantics not guaranteed if `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		require -- from TRAVERSABLE
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_TWO_WAY_SORTED_SET [G]
		do
			if attached {FLAT_TWO_WAY_SORTED_SET [G]} Current as acs then
				cs := acs
				c := acs.cursor
			end
			from
				start
				Result := True
			until
				after or not Result
			loop
				Result := test.item ([item])
				forth
			end
			if cs /= Void and c /= Void then
				cs.go_to (c)
			end
		ensure
			definition: Result = sequence.range.for_all (test)
		end

	there_exists (test: PREDICATE [ANY, TUPLE [G]]): BOOLEAN
			-- Is `test' true for at least one item?
			-- Semantics not guaranteed if `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		require
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_TWO_WAY_SORTED_SET [G]
		do
			if attached {FLAT_TWO_WAY_SORTED_SET [G]} Current as acs then
				cs := acs
				c := acs.cursor
			end
			from
				start
			until
				after or Result
			loop
				Result := test.item ([item])
				forth
			end
			if cs /= Void and c /= Void then
				cs.go_to (c)
			end
		ensure
			definition: Result = sequence.range.exists (test)
		end

invariant

	sequence_sorted: sequence.but_last.domain.for_all (agent (i: INTEGER): BOOLEAN
		do
			Result := sequence [i] < sequence [i + 1]
		end)

	map_domain_definition: map.domain |=| sequence.domain
	map_definition: map.domain.for_all (agent (i: INTEGER): BOOLEAN
		do
			Result := map [i] = sequence [i]
		end)
	lower_definition: lower = 1

	count_definition: count = sequence.count
	after_definition: after = (index > sequence.count)
	before_definition: before = (index < 1)


end -- class TWO_WAY_SORTED_SET

