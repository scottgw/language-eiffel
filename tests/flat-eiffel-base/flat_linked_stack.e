note
	model: sequence, object_comparison

class
	FLAT_LINKED_STACK [G]

inherit
	V_EQUALITY [G]
		redefine
			copy,
			is_equal
		end

create
	make

feature -- Initialization

	make
			-- Create an empty list.
			-- (from LINKED_LIST)
		do
			before := True
		ensure
			sequence_effect: sequence.is_empty
			object_comparison_effect: not object_comparison
		end

feature -- Access

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
		ensure
			definition: Result = sequence.range.exists (agent element_equal (v, ?))
		end

	item: G
			-- Item at the first position
		require
			readable: readable
		local
			f: like first_element
		do
			check
				not_empty: not is_empty
			end
			f := first_element
			check
				f_attached: f /= Void
			end
			Result := f.item
		ensure
			definition: Result = sequence.first
		end

feature {FLAT_LINKED_STACK} -- Access

	cursor: FLAT_LINKED_LIST_CURSOR [G]
			-- Current cursor position
			-- (from LINKED_LIST)
		do
			create Result.make (active, after, before)
		ensure -- from CURSOR_STRUCTURE
			cursor_not_void: Result /= Void
		end

	first_element: like new_cell
			-- Head of list
			-- (from LINKED_LIST)

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

	ll_item: G
			-- Current item
			-- (from LINKED_LIST)
		require
			not_off: not off
		local
			a: like active
		do
			a := active
			check
				a_attached: a /= Void
			end
			Result := a.item
		end

feature {NONE} -- Access

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
		end

	sequential_has (v: like ll_item): BOOLEAN
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

	i_th alias "[]" (i: INTEGER_32): like item
			-- Item at `i'-th position
			-- Was declared in CHAIN as synonym of at.
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
		end

	sequential_index_of (v: like ll_item; i: INTEGER_32): INTEGER_32
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
					if ll_item ~ v then
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
					if ll_item = v then
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

	item_for_iteration: G
			-- Item at current position
			-- (from LINEAR)
		require
			not_off: not off
		do
			Result := ll_item
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
		end

	sequential_occurrences (v: like ll_item): INTEGER_32
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

	sequential_search (v: like ll_item)
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
					exhausted or else v ~ ll_item
				loop
					forth
				end
			else
				from
				until
					exhausted or else v = ll_item
				loop
					forth
				end
			end
		end

feature -- Measurement

	count: INTEGER_32
			-- Number of items
			-- (from LINKED_LIST)

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
			definition: Result = occurrences_that (sequence, agent element_equal (v, ?))
		end

feature {NONE} -- Measurement

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
			-- (from CHAIN)
		do
			create Result.make (1, count)
		end

feature -- Comparison

	is_equal (other: FLAT_LINKED_STACK [G]): BOOLEAN
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
				sequence.count = other.sequence.count and
				sequence.domain.for_all (agent (i: INTEGER; o: FLAT_LINKED_STACK [G]): BOOLEAN
					do
						Result := element_equal (o.sequence [i], sequence [i])
					end (?, other)))
		end

feature -- Status report

	changeable_comparison_criterion: BOOLEAN
			-- May object_comparison be changed?
			-- (Answer: yes by default.)
			-- (from CONTAINER)
		do
			Result := True
		ensure
			definition: Result
		end

	extendible: BOOLEAN
			-- May new items be added? (Answer: yes.)
			-- (from DYNAMIC_CHAIN)
		do
			Result := True
		ensure
			definition: Result
		end

	Full: BOOLEAN = False
			-- Is structured filled to capacity? (Answer: no.)
			-- (from LINKED_LIST)

	is_empty: BOOLEAN
			-- Is structure empty?
			-- (from FINITE)
		do
			Result := (count = 0)
		ensure
			definition: Result = sequence.is_empty
		end

	is_inserted (v: G): BOOLEAN
			-- Has `v' been inserted by the most recent insertion?
			-- (By default, the value returned is equivalent to calling
			-- `has (v)'. However, descendants might be able to provide more
			-- efficient implementations.)
			-- (from COLLECTION)
		do
			Result := has (v)
		ensure
			definition: Result = sequence.range.exists (agent element_equal (v, ?))
		end

	object_comparison: BOOLEAN
			-- Must search operations use equal rather than `='
			-- for comparing references? (Default: no, use `='.)
			-- (from CONTAINER)

	prunable: BOOLEAN
			-- May items be removed? (Answer: yes.)
			-- (from DYNAMIC_CHAIN)
		do
			Result := True
		ensure
			definition: Result
		end

	readable: BOOLEAN
			-- Is there a current item that may be read?
			-- (from DISPENSER)
		do
			Result := not is_empty
		ensure
			definition: Result = not sequence.is_empty
		end

	replaceable: BOOLEAN
			-- Can current item be replaced?
			-- (from ACTIVE)
		do
			Result := True
		ensure
			definition: Result
		end

	writable: BOOLEAN
			-- Is there a current item that may be modified?
			-- (from DISPENSER)
		do
			Result := not is_empty
		ensure
			definition: Result = not sequence.is_empty
		end

feature {FLAT_LINKED_STACK} -- Status report

	before: BOOLEAN
			-- Is there no valid cursor position to the left of cursor?
			-- (from LINKED_LIST)

	exhausted: BOOLEAN
			-- Has structure been completely explored?
			-- (from LINEAR)
		do
			Result := off
		end

	isfirst: BOOLEAN
			-- Is cursor at first position?
			-- (from LINKED_LIST)
		do
			Result := not after and not before and (active = first_element)
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

	valid_cursor_index (i: INTEGER_32): BOOLEAN
			-- Is `i' correctly bounded for cursor movement?
			-- (from CHAIN)
		do
			Result := (i >= 0) and (i <= count + 1)
		end

	valid_index (i: INTEGER_32): BOOLEAN
			-- Is `i' within allowable bounds?
			-- (from CHAIN)
		do
			Result := (i >= 1) and (i <= count)
		end

feature {FLAT_LINKED_STACK} -- Status report

	after: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?
			-- (from LINKED_LIST)

	off: BOOLEAN
			-- Is there no current item?
			-- (from LINKED_LIST)
		do
			Result := after or before
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

feature -- Status setting

	compare_objects
			-- Ensure that future search operations will use equal
			-- rather than `=' for comparing references.
			-- (from CONTAINER)
		note
			modify: object_comparison
		require
			changeable_comparison_criterion: changeable_comparison_criterion
		do
			object_comparison := True
		ensure
			object_comparison_effect: object_comparison
		end

	compare_references
			-- Ensure that future search operations will use `='
			-- rather than equal for comparing references.
			-- (from CONTAINER)
		note
			modify: object_comparison
		require
			changeable_comparison_criterion: changeable_comparison_criterion
		do
			object_comparison := False
		ensure
			object_comparison_effect: not object_comparison
		end

feature {FLAT_LINKED_STACK} -- Cursor movement

	forth
			-- Move cursor to next position.
			-- (from LINKED_LIST)
		require -- from LINEAR
			not_after: not after
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

	go_to (p: FLAT_CURSOR)
			-- Move cursor to position `p'.
			-- (from LINKED_LIST)
		require -- from CURSOR_STRUCTURE
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

	back
			-- Move to previous item.
			-- (from LINKED_LIST)
		require -- from BILINEAR
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
		end

	finish
			-- Move cursor to last position.
			-- (Go before if empty)
			-- (from LINKED_LIST)
		require -- from  LINEAR
			True
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
		end

	go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th position.
			-- (from LINKED_LIST)
		require -- from CHAIN
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

	move (i: INTEGER_32)
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

	search (v: like ll_item)
			-- Move to first position (at or after current
			-- position) where item and `v' are equal.
			-- If structure does not include `v' ensure that
			-- exhausted will be true.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from BILINEAR)
		do
			if before and not is_empty then
				forth
			end
			sequential_search (v)
		end

feature -- Element change

	append (s: FLAT_LINKED_STACK [G])
			-- Append a copy of `s'.
			-- (Synonym for fill)
			-- (from DISPENSER)
		note
			modify: sequence
		require
			argument_not_void: s /= Void
		do
			fill (s)
		ensure
			sequence_effect: sequence |=| (s.sequence + old sequence)
		end

	extend (v: like item)
			-- Push `v' onto top.
		note
			modify: sequence
		require
			extendible: extendible
		do
			put_front (v)
		ensure
			sequence_effect: sequence |=| old sequence.prepended (v)
		end

	fill (other: FLAT_LINKED_STACK [G])
			-- Fill with as many items of `other' as possible.
			-- Fill items with greatest index from `other' first.
			-- Items inserted with lowest index (from `other') will
			-- always be on the top of stack.
			-- The representations of `other' and current structure
			-- need not be the same.
			-- (from STACK)
		note
			modify: sequence
		require -- from COLLECTION
			other_not_void: other /= Void
			extendible: extendible
		local
			temp: ARRAYED_STACK [G]
		do
			create temp.make (0)
			from
				other.start
			until
				other.off
			loop
				temp.extend (other.item)
				other.forth
			end
			from
			until
				temp.is_empty or else not extendible
			loop
				extend (temp.item)
				temp.remove
			end
		ensure
			sequence_effect: sequence |=| (other.sequence + old sequence)
		end

	force (v: like item)
			-- Push `v' onto top.
		note
			modify: sequence
		require -- from DISPENSER
			extendible: extendible
		do
			put_front (v)
		ensure
			sequence_effect: sequence |=| old sequence.prepended (v)
		end

	put (v: like item)
			-- Push `v' onto top.
		note
			modify: sequence
		require -- from COLLECTION
			extendible: extendible
		do
			put_front (v)
		ensure
			sequence_effect: sequence |=| old sequence.prepended (v)
		end

	replace (v: like item)
			-- Replace current item by `v'.
			-- (from LINKED_LIST)
		note
			modify: sequence
		require -- from ACTIVE
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
			sequence_effect: sequence |=| old sequence.replaced_at (1, v)
		end

feature {FLAT_LINKED_STACK} -- Element change

	put_front (v: like item)
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

	put_right (v: like item)
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
		ensure -- from DYNAMIC_CHAIN
			new_count: count = old count + 1
			same_index: index = old index
--		ensure then -- from LINKED_LIST
			next_exists: next /= Void
			item_inserted: not old before implies (attached next as n and then n.item = v)
			item_inserted_before: old before implies (attached active as c and then c.item = v)
		end

feature -- Removal

	remove
			-- Remove item on top.
		note
			modify: sequence
		require
			prunable: prunable
			writable: writable
		do
			start
			ll_remove
		ensure
			sequence_effect: sequence |=| old sequence.but_first
		end

	wipe_out
			-- Remove all items.
			-- (from LINKED_LIST)
		note
			modify: sequence
		require
			prunable: prunable
		do
			internal_wipe_out
		ensure
			sequence_effect: sequence.is_empty
		end

feature {NONE} -- Removal

	ll_remove
			-- Remove current item.
			-- Move cursor to right neighbor
			-- (or after if no right neighbor).
			-- (from LINKED_LIST)
		require -- from ACTIVE
			prunable: prunable
			writable: writable
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
		ensure then -- from DYNAMIC_LIST
			after_when_empty: is_empty implies after
		end

feature -- Conversion

	linear_representation: FLAT_ARRAYED_LIST [G]
			-- Representation as a linear structure
			-- (order is reverse of original order of insertion)
		local
			old_cursor: FLAT_CURSOR
		do
			old_cursor := cursor
			from
				create Result.make (count)
				start
			until
				after
			loop
				Result.extend (ll_item)
				forth
			end
			go_to (old_cursor)
		ensure
			sequence_definition: Result.sequence |=| sequence
			object_comparison_definition: Result.object_comparison = object_comparison
		end

feature -- Duplication

	copy (other: FLAT_LINKED_STACK [G])
			-- Update current object using fields of object attached
			-- to `other', so as to yield equal objects.
			-- (from LINKED_LIST)
		note
			modify: sequence, object_comparison
		local
			cur: FLAT_LINKED_LIST_CURSOR [G]
		do
			if other /= Current then
				standard_copy (other)
				if not other.is_empty then
					internal_wipe_out
					if attached {FLAT_LINKED_LIST_CURSOR [G]} other.cursor as l_cur then
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

	duplicate (n: INTEGER_32): like Current
			-- New stack containing the `n' latest items inserted
			-- in current stack.
			-- If `n' is greater than count, identical to current stack.
		require
			valid_subchain: n >= 0
		local
			counter: INTEGER_32
			old_cursor: FLAT_CURSOR
			list: FLAT_LINKED_STACK [G]
		do
			if not is_empty then
				old_cursor := cursor
				from
					create Result.make
					list := Result
					start
				until
					after or counter = n
				loop
					list.finish
					list.put_right (ll_item)
					counter := counter + 1
					forth
				end
				go_to (old_cursor)
			end
		ensure
			sequence_definition: Result.sequence |=| sequence.front (n)
			object_comparison_definition: not Result.object_comparison
		end

feature {NONE} -- Implementation

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

	new_chain: FLAT_LINKED_STACK [G]
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

feature {FLAT_LINKED_STACK} -- Implementation

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

feature -- Specification

	sequence: MML_SEQUENCE [G]
			-- Sequence of elements.
		note
			status: specification
		local
			c: like first_element
		do
			from
				create Result
				c := first_element
			until
				c = Void
			loop
				Result := Result & c.item
				c := c.right
			end
		end

	element_equal (x, y: G): BOOLEAN
			-- Element equality according to `object_comparison'.
		note
			status: specification
		do
			if object_comparison then
				Result := object_equal (x, y)
			else
				Result := reference_equal (x, y)
			end
		ensure
			definition_object_comparison: object_comparison implies Result = object_equal (x, y)
			definition_reference_comparison: not object_comparison implies Result = reference_equal (x, y)
		end

	occurrences_that (s: MML_SEQUENCE [G]; pred: PREDICATE [ANY, TUPLE [G]]): INTEGER
			-- Number of elements of `s' that satisfy `pred'.
		note
			status: specification
		do
			Result := s.inverse.restricted (s.range | pred).count
		ensure
			definition: Result = s.inverse.restricted (s.range | pred).count
		end

invariant

	count_definition: count = sequence.count

end -- class LINKED_STACK

