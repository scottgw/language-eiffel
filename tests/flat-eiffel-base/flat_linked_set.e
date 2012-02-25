note
	model: sequence, index, object_comparison

class
	FLAT_LINKED_SET [G]

inherit
	FLAT_LINEAR_SUBSET [G]
		redefine
			copy,
			is_equal,
			fill,
			prune_all
		end

	FLAT_READABLE_INDEXABLE [G]
		rename
			item as i_th alias "[]"
		redefine
			copy,
			is_equal,
			new_cursor
		end

create
	make

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

	cursor: FLAT_LINKED_SET_CURSOR [G]
			-- Current cursor position
			-- (from LINKED_LIST)
		do
			create Result.make (active, after, before)
		ensure
			cursor_not_void: Result /= Void
		end

	first: like item
			-- Item at first position
			-- (from LINKED_LIST)
		require -- from CHAIN
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

	has (v: like item): BOOLEAN
			-- Does chain include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from CHAIN)
		local
			pos: FLAT_CURSOR
		do
			pos := cursor
			Result := sequential_has (v)
			go_to (pos)
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
		require -- from LINEAR
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

feature {FLAT_LINKED_SET, FLAT_LINKED_SET_ITERATION_CURSOR} -- Access

	first_element: like new_cell
			-- Head of list
			-- (from LINKED_LIST)

feature {NONE} -- Access

	sequential_has (v: like item): BOOLEAN
			-- Does structure include an occurrence of `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		require -- from  CONTAINER
			True
		do
			start
			if not off then
				search (v)
			end
			Result := not exhausted
		ensure -- from CONTAINER
			not_found_in_empty: Result implies not is_empty
		end

	sequential_index_of (v: like item; i: INTEGER_32): INTEGER_32
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
		ensure -- from LINEAR
			non_negative_result: Result >= 0
		end

	sequential_occurrences (v: like item): INTEGER_32
			-- Number of times `v' appears.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		require -- from  BAG
			True
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
		ensure -- from BAG
			non_negative_occurrences: Result >= 0
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
		ensure -- from LINEAR
			object_found: (not exhausted and object_comparison) implies v ~ item
			item_found: (not exhausted and not object_comparison) implies v = item
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

	is_equal (other: FLAT_LINKED_SET [G]): BOOLEAN
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
			-- Note: compares sequences and not sets, which could be considered ok or not...
			definition: Result = (object_comparison = other.object_comparison and
				sequence.count = other.sequence.count and
				sequence.domain.for_all (agent (i: INTEGER; o: FLAT_LINKED_SET [G]): BOOLEAN
					do
						Result := element_equal (o.sequence [i], sequence [i])
					end (?, other)))
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
			-- (from LINKED_LIST)
		local
			a: like active
		do
			if not after and then not before then
				a := active
				Result := a /= Void and then a.right = Void
			end
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
			-- Move to previous item.
			-- (from LINKED_LIST)
		note
			modify: index
		require
			not_before: not before
		do
			if is_empty then
				before := True
				after := False
			elseif after then
				after := False
			elseif isfirst then
				before := True
			else
				active := previous
			end
		ensure
			index_effect: index = old index - 1
		end

	finish
			-- Move cursor to last position.
			-- (Go before if empty)
			-- (from LINKED_LIST)
		note
			modify: index
		local
			p: like new_cell
		do
			from
				p := active
			until
				p = Void
			loop
				active := p
				p := p.right
			end
			after := False
			before := (active = Void)
		ensure
			index_effect: index = sequence.count
		end

	forth
			-- Move cursor to next position.
			-- (from LINKED_LIST)
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
				if a /= Void and then a.right /= Void then
					active := a.right
				else
					after := True
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
			-- may end up off if the offset is too big.
			-- (from LINKED_LIST)
		note
			modify: index
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

feature -- Element change

	append (s: FLAT_LINKED_SET [G])
			-- Append a copy of `s'.
			-- (from CHAIN)
		note
			modify: sequence
		require
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
			sequence_effect: sequence |=| old (sequence +
				s.sequence.removed (s.sequence.inverse.restricted (s.set | agent set_has (set, ?)).range))
		end

	extend (v: G)
			-- Ensure that set includes `v'.
			-- Was declared in LINKED_SET as synonym of put.
		do
			if is_empty or else not has (v) then
				ll_extend (v)
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

	merge_left (other: FLAT_LINKED_SET [G])
			-- Merge `other' into current structure before cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from LINKED_LIST)
		note
			modify: sequence, index, other_sequence, other_index
		require
			extendible: extendible
			not_before: not before
			other_exists: other /= Void
			not_current: other /= Current
		local
			other_first_element: like first_element
			other_last_element: like last_element
			other_count: INTEGER_32
			p: like first_element
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
				if is_empty then
					first_element := other_first_element
					active := first_element
				elseif isfirst then
					other_last_element.put_right (first_element)
					first_element := other_first_element
				else
					p := previous
					if p /= Void then
						p.put_right (other_first_element)
					end
					other_last_element.put_right (active)
				end
				count := count + other_count
			end
		ensure
			sequence_effect: sequence |=| old (sequence.front (index - 1) + other.sequence + sequence.tail (index))
			index_effect: index = old index + old other.sequence.count
			other_sequence_effect: other.sequence.is_empty
		end

	merge_right (other: FLAT_LINKED_SET [G])
			-- Merge `other' into current structure after cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from LINKED_LIST)
		note
			modify: sequence, index, other_sequence, other_index
		require
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
		ensure
			sequence_effect: sequence |=| old (sequence.front (index) + other.sequence + sequence.tail (index + 1))
			index_unchanged: index = old index
			other_sequence_effect: other.sequence.is_empty
		end

	put (v: G)
			-- Ensure that set includes `v'.
			-- Was declared in LINKED_SET as synonym of extend.
		do
			if is_empty or else not has (v) then
				ll_extend (v)
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
			-- (from LINKED_LIST)
		note
			modify: sequence
		require
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
			-- (from LINKED_LIST)
		local
			p: like new_cell
			a: like active
		do
			a := active
			if a = Void then
				put_front (v)
			elseif after then
				back
				put_right (v)
				move (2)
			else
				p := new_cell (a.item)
				p.put_right (a.right)
				a.put (v)
				a.put_right (p)
				active := p
				count := count + 1
			end
		end

	put_right (v: like item)
			-- Add `v' to the right of cursor position.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		note
			modify: sequence
		require
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

	ll_extend (v: like item)
			-- Add `v' to end.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		require -- from COLLECTION
			extendible: extendible
		local
			p: like first_element
			l: like last_element
		do
			p := new_cell (v)
			if is_empty then
				first_element := p
				active := p
			else
				l := last_element
				if l /= Void then
					l.put_right (p)
					if after then
						active := p
					end
				end
			end
			count := count + 1
		ensure -- from COLLECTION
			item_inserted: is_inserted (v)
		end

	ll_put (v: like item)
			-- Replace current item by `v'.
			-- (Synonym for replace)
			-- (from CHAIN)
		require -- from CHAIN
			writeable: writable
			replaceable: replaceable
		do
			replace (v)
		ensure -- from CHAIN
			same_count: count = old count
			is_inserted: is_inserted (v)
		end

feature -- Removal

	prune (v: like item)
			-- Remove `v' if present.
		do
			start
			ll_prune (v)
		end

	prune_all (v: like item)
			-- Remove all occurrences of `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- Leave structure exhausted.
			-- (from DYNAMIC_CHAIN)
		do
			from
				start
				search (v)
			until
				exhausted
			loop
				remove
				search (v)
			end
		ensure then
			index_effect: index = sequence.count + 1
		end

	remove
			-- Remove current item.
			-- Move cursor to right neighbor
			-- (or after if no right neighbor).
			-- (from LINKED_LIST)
		local
			succ: like first_element
			removed: like active
			a: like active
			p: like previous
		do
			removed := active
			if removed /= Void then
				if isfirst then
					first_element := removed.right
					removed.forget_right
					active := first_element
					if count = 1 then
						check
							no_active: active = Void
						end
						after := True
					end
				elseif islast then
					active := previous
					a := active
					if a /= Void then
						a.forget_right
					end
					after := True
				else
					succ := removed.right
					p := previous
					if p /= Void then
						p.put_right (succ)
					end
					removed.forget_right
					active := succ
				end
				count := count - 1
				cleanup_after_remove (removed)
			end
		end

	remove_left
			-- Remove item to the left of cursor position.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		note
			modify: sequence, index
		require
			left_exists: index > 1
		do
			move (-2)
			remove_right
			forth
		ensure
			sequence_effect: sequence |=| old sequence.removed_at (index - 1)
			index_effect: index = old index - 1
		end

	remove_right
			-- Remove item to the right of cursor position.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		note
			modify: sequence
		require
			right_exists: index < count
		local
			removed: like first_element
			f: like first_element
			a: like active
			succ: like active
		do
			if before then
				f := first_element
				if f /= Void then
					removed := f
					first_element := f.right
					a := active
					if a /= Void then
						a.forget_right
					end
					active := first_element
				end
			else
				a := active
				if a /= Void then
					succ := a.right
					if succ /= Void then
						removed := succ
						a.put_right (succ.right)
						succ.forget_right
					end
				end
			end
			count := count - 1
			cleanup_after_remove (removed)
		ensure
			sequence_effect: sequence |=| old sequence.removed_at (index + 1)
		end

	wipe_out
			-- Remove all items.
			-- (from LINKED_LIST)
		do
			internal_wipe_out
		end

feature {NONE} -- Removal

	ll_prune (v: like item)
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
		ensure -- from COLLECTION
			wiped_out: is_empty
		end

feature -- Transformation

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

	linear_representation: FLAT_LINKED_SET [G]
			-- Representation as a linear structure
			-- (from LINEAR)
		do
			Result := Current
		end

feature -- Duplication

	copy (other: FLAT_LINKED_SET [G])
			-- Update current object using fields of object attached
			-- to `other', so as to yield equal objects.
			-- (from LINKED_LIST)
		note
			modify: sequence, index, object_comparison
		local
			cur: FLAT_LINKED_SET_CURSOR [G]
		do
			if other /= Current then
				standard_copy (other)
				if not other.is_empty then
					internal_wipe_out
					if attached {FLAT_LINKED_SET_CURSOR [G]} other.cursor as l_cur then
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
		end

	duplicate (n: INTEGER_32): FLAT_LINKED_SET [G]
			-- Copy of sub-chain beginning at current position
			-- and having min (`n', `from_here') items,
			-- where `from_here' is the number of items
			-- at or to the right of current position.
			-- (from DYNAMIC_CHAIN)
--		require -- from CHAIN
--			not_off_unless_after: off implies after
--			valid_subchain: n >= 0
		local
			pos: FLAT_CURSOR
			counter: INTEGER_32
		do
			from
				Result := new_chain
				if object_comparison then
					Result.compare_objects
				end
				pos := cursor
			until
				(counter = n) or else exhausted
			loop
				Result.extend (item)
				forth
				counter := counter + 1
			end
			go_to (pos)
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

feature {FLAT_LINKED_SET} -- Implementation

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

	last_element: like first_element
			-- Tail of list
			-- (from LINKED_LIST)
		local
			p: like first_element
		do
			from
				p := active
			until
				p = Void
			loop
				Result := p
				p := p.right
			end
		end

	new_cell (v: like item): FLAT_LINKABLE [like item]
			-- A newly created instance of the same type as first_element.
			-- This feature may be redefined in descendants so as to
			-- produce an adequately allocated and initialized object.
			-- (from LINKED_LIST)
		do
			create Result.put (v)
		ensure -- from LINKED_LIST
			result_exists: Result /= Void
		end

	new_chain: FLAT_LINKED_SET [G]
			-- A newly created instance of the same type.
			-- This feature may be redefined in descendants so as to
			-- produce an adequately allocated and initialized object.
			-- (from LINKED_LIST)
		do
			create Result.make
		ensure -- from DYNAMIC_CHAIN
			result_exists: Result /= Void
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

	previous: like first_element
			-- Element left of cursor
			-- (from LINKED_LIST)
		local
			p: like first_element
		do
			if after then
				Result := active
			elseif not (isfirst or before) then
				from
					p := first_element
				until
					p = Void or else p.right = active
				loop
					p := p.right
				end
				Result := p
			end
		end

feature -- Access: Cursor

	new_cursor: FLAT_LINKED_SET_ITERATION_CURSOR [G]
			-- Fresh cursor associated with current structure
			-- (from LINKED_LIST)
		do
			create Result.make (Current)
			Result.start
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
			cs: FLAT_LINKED_SET [G]
		do
			if attached {FLAT_LINKED_SET [G]} Current as acs then
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
			cs: FLAT_LINKED_SET [G]
		do
			if attached {FLAT_LINKED_SET [G]} Current as acs then
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
		require
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_LINKED_SET [G]
		do
			if attached {FLAT_LINKED_SET [G]} Current as acs then
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
			cs: FLAT_LINKED_SET [G]
		do
			if attached {FLAT_LINKED_SET [G]} Current as acs then
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

	map_domain_definition: map.domain |=| sequence.domain
	map_definition: map.domain.for_all (agent (i: INTEGER): BOOLEAN
		do
			Result := map [i] = sequence [i]
		end)
	lower_definition: lower = 1

	count_definition: count = sequence.count
	after_definition: after = (index > sequence.count)
	before_definition: before = (index < 1)

end -- class LINKED_SET

