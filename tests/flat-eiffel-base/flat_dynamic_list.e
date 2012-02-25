note
	model: sequence, index, object_comparison

deferred class
	FLAT_DYNAMIC_LIST [G]

inherit
	FLAT_READABLE_INDEXABLE [G]
		rename
			item as i_th alias "[]"
		redefine
			is_equal
		end

	V_EQUALITY [G]
		redefine
			is_equal
		end

	V_ORDER [INTEGER]
		redefine
			is_equal
		end

feature -- Access

	at alias "@" (i: INTEGER_32): like item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in CHAIN as synonym of i_th.
			-- (from CHAIN)
		require
			valid_index: valid_index (i)
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

	cursor: FLAT_CURSOR
			-- Current cursor position
			-- (from CURSOR_STRUCTURE)
		deferred
		ensure
			-- ToDo: model for FLAT_CURSOR
			cursor_not_void: Result /= Void
		end

	first: like item
			-- Item at first position
			-- (from CHAIN)
		require
			not_empty: not is_empty
		local
			pos: FLAT_CURSOR
		do
			pos := cursor
			start
			Result := item
			go_to (pos)
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
		ensure
			definition: Result = sequence.range.exists (agent element_equal (v, ?))
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
			-- (from LINEAR)
		deferred
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
			definition_has: occurrences_that (sequence, agent element_equal (v, ?)) >= i implies
				element_equal (v, sequence [Result]) and occurrences_that (sequence.front (Result), agent element_equal (v, ?)) = i
			definition_not_has: occurrences_that (sequence, agent element_equal (v, ?)) < i implies Result = 0
		end

	item: G
			-- Current item
			-- (from ACTIVE)
		require
			not_off: not off
		deferred
		ensure
			definition: Result = sequence [index]
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
			-- (from CHAIN)
		require
			not_empty: not is_empty
		local
			pos: FLAT_CURSOR
		do
			pos := cursor
			finish
			Result := item
			go_to (pos)
		ensure
			definition: Result = sequence.last
		end

feature {NONE} -- Access

	sequential_has (v: like item): BOOLEAN
			-- Does structure include an occurrence of `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		note
			modify: index
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
		note
			modify: index
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
		end

	sequential_occurrences (v: like item): INTEGER_32
			-- Number of times `v' appears.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from LINEAR)
		note
			modify: index
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
		note
			modify: index
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

feature -- Measurement

	count: INTEGER_32
			-- Number of items
			-- (from FINITE)
		deferred
		ensure
			definition: Result = sequence.count
		end

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
			definition: Result = occurrences_that (sequence, agent element_equal (v, ?))
		end

feature -- Comparison

	is_equal (other: FLAT_DYNAMIC_LIST [G]): BOOLEAN
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
				sequence.domain.for_all (agent (i: INTEGER; o: FLAT_DYNAMIC_LIST [G]): BOOLEAN
					do
						Result := element_equal (o.sequence [i], sequence [i])
					end (?, other)))
		end

feature -- Status report

	after: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?
			-- (from LIST)
		do
			Result := (index = count + 1)
		ensure
			definition: Result = (index = sequence.count + 1)
		end

	before: BOOLEAN
			-- Is there no valid cursor position to the left of cursor?
			-- (from LIST)
		do
			Result := (index = 0)
		ensure
			definition: Result = (index = 0)
		end

	changeable_comparison_criterion: BOOLEAN
			-- May object_comparison be changed?
			-- (Answer: yes by default.)
			-- (from CONTAINER)
		do
			Result := True
		ensure
			definition: Result
		end

	exhausted: BOOLEAN
			-- Has structure been completely explored?
			-- (from LINEAR)
		do
			Result := off
		ensure -- from LINEAR
			definition: Result = not sequence.domain [index]
		end

	extendible: BOOLEAN
			-- May new items be added? (Answer: yes.)
			-- (from DYNAMIC_CHAIN)
		do
			Result := True
		ensure
			definition: Result
		end

--	full: BOOLEAN
--			-- Is structure filled to capacity?
--			-- (from BOX)
--		deferred
--		end

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
		deferred
--		do
--			Result := has (v)
		-- Changed to deferred, because of different semantics in all observable descendants
		ensure
			definition: Result = ((not sequence.is_empty and then v = sequence.last) or (sequence.domain [index] and then v = sequence [index]))
		end

	isfirst: BOOLEAN
			-- Is cursor at first position?
			-- (from CHAIN)
		do
			Result := not is_empty and (index = 1)
		ensure
			definition: Result = (not sequence.is_empty and index = 1)
		end

	islast: BOOLEAN
			-- Is cursor at last position?
			-- (from CHAIN)
		do
			Result := not is_empty and (index = count)
		ensure
			definition: Result = (not sequence.is_empty and index = count)
		end

	object_comparison: BOOLEAN
			-- Must search operations use equal rather than `='
			-- for comparing references? (Default: no, use `='.)
			-- (from CONTAINER)

	off: BOOLEAN
			-- Is there no current item?
			-- (from CHAIN)
		do
			Result := (index = 0) or (index = count + 1)
		ensure
			definition: Result = not sequence.domain [index]
		end

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
			-- (from SEQUENCE)
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
			-- (from CURSOR_STRUCTURE)
		deferred
			-- ToDo: model for FLAT_CURSOR
		end

	valid_cursor_index (i: INTEGER_32): BOOLEAN
			-- Is `i' correctly bounded for cursor movement?
			-- (from CHAIN)
		do
			Result := (i >= 0) and (i <= count + 1)
		ensure -- from CHAIN
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

feature -- Status setting

	compare_objects
			-- Ensure that future search operations will use equal
			-- rather than `=' for comparing references.
			-- (from CONTAINER)
		note
			modify: object_comparison
		require -- from CONTAINER
			changeable_comparison_criterion: changeable_comparison_criterion
		do
			object_comparison := True
		ensure -- from CONTAINER
			object_comparison_effect: object_comparison
		end

	compare_references
			-- Ensure that future search operations will use `='
			-- rather than equal for comparing references.
			-- (from CONTAINER)
		note
			modify: object_comparison
		require -- from CONTAINER
			changeable_comparison_criterion: changeable_comparison_criterion
		do
			object_comparison := False
		ensure -- from CONTAINER
			object_comparison_effect: not object_comparison
		end

feature -- Cursor movement

	back
			-- Move to previous position.
			-- (from BILINEAR)
		note
			modify: index
		require -- from BILINEAR
			not_before: not before
		deferred
		ensure
			index_effect: index = old index - 1
		end

	finish
			-- Move cursor to last position.
			-- (No effect if empty)
			-- (from CHAIN)
		note
			modify: index
		do
			if not is_empty then
				go_i_th (count)
			end
		ensure
			index_effect: index = sequence.count
		end

	forth
			-- Move to next position; if no next position,
			-- ensure that exhausted will be true.
			-- (from LIST)
		note
			modify: index
		require
			not_after: not after
		deferred
		ensure
			index_effect: index = old index + 1
		end

	go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th position.
			-- (from CHAIN)
		note
			modify: index
		require
			valid_cursor_index: valid_cursor_index (i)
		do
			move (i - index)
		ensure
			index_effect: index = i
		end

	go_to (p: FLAT_CURSOR)
			-- Move cursor to position `p'.
			-- (from CURSOR_STRUCTURE)
		note
			modify: index
		require
			cursor_position_valid: valid_cursor (p)
		deferred
			-- ToDo: model for FLAT_CURSOR
		end

	move (i: INTEGER_32)
			-- Move cursor `i' positions. The cursor
			-- may end up off if the absolute value of `i'
			-- is too big.
			-- (from CHAIN)
		note
			modify: index
		local
			counter, pos, final: INTEGER_32
		do
			if i > 0 then
				from
				until
					(counter = i) or else after
				loop
					forth
					counter := counter + 1
				end
			elseif i < 0 then
				final := index + i
				if final <= 0 then
					start
					back
				else
					from
						start
						pos := 1
					until
						pos = final
					loop
						forth
						pos := pos + 1
					end
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
			-- (No effect if empty)
			-- (from CHAIN)
		note
			modify: index
		do
			if not is_empty then
				go_i_th (1)
			end
		ensure
			index_effect: index = 1
		end

feature -- Element change

	append (s: FLAT_DYNAMIC_LIST [G])
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
			sequence_effect: sequence |=| old (sequence + s.sequence)
		end

	extend (v: G)
			-- Add a new occurrence of `v'.
			-- (from BAG)
		note
			modify: sequence
		deferred
		ensure
			sequence_effect: sequence |=| (old sequence & v)
		end

	fill (other: FLAT_DYNAMIC_LIST [G])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current structure
			-- need not be the same.
			-- (from CHAIN)
		note
			modify: sequence
		require -- from COLLECTION
			other_not_void: other /= Void
		local
			lin_rep: FLAT_DYNAMIC_LIST [G]
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
		ensure
			sequence_effect: sequence |=| old (sequence + other.sequence)
		end

	force (v: like item)
			-- Add `v' to end.
			-- (from SEQUENCE)
		note
			modify: sequence
		do
			extend (v)
		ensure
			sequence_effect: sequence |=| (old sequence & v)
		end

	merge_left (other: like Current)
			-- Merge `other' into current structure before cursor
			-- position. Do not move cursor. Empty `other'.
		note
			modify: sequence, index, other_sequence, other_index
		require -- from DYNAMIC_CHAIN
			not_before: not before
			other_exists: other /= Void
			not_current: other /= Current
		do
			from
				other.start
			until
				other.is_empty
			loop
				put_left (other.item)
				other.remove
			end
		ensure
			sequence_effect: sequence |=| old (sequence.front (index - 1) + other.sequence + sequence.tail (index))
			index_effect: index = old index + old other.sequence.count
			other_sequence_effect: other.sequence.is_empty
		end

	merge_right (other: like Current)
			-- Merge `other' into current structure after cursor
			-- position. Do not move cursor. Empty `other'.
		note
			modify: sequence, index, other_sequence, other_index
		require
			not_after: not after
			other_exists: other /= Void
			not_current: other /= Current
		do
			from
				other.finish
			until
				other.is_empty
			loop
				put_right (other.item)
				other.back
				other.remove_right
			end
		ensure
			sequence_effect: sequence |=| old (sequence.front (index) + other.sequence + sequence.tail (index + 1))
			index_unchanged: index = old index
			other_sequence_effect: other.sequence.is_empty
		end

	put (v: like item)
			-- Replace current item by `v'.
			-- (Synonym for replace)
			-- (from CHAIN)
		note
			modify: sequence
		require
			writeable: writable
		do
			replace (v)
		ensure
			sequence_effect: sequence |=| old sequence.replaced_at (index, v)
		end

	sequence_put (v: like item)
			-- Add `v' to end.
			-- (from SEQUENCE)
		note
			modify: sequence
		do
			extend (v)
		ensure
			sequence_effect: sequence |=| old (sequence & v)
		end

	put_front (v: like item)
			-- Add `v' at beginning.
			-- Do not move cursor.
			-- (from DYNAMIC_CHAIN)
		note
			modify: sequence
		deferred
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
		note
			modify: sequence, index
		require
			not_before: not before
		local
			temp: like item
		do
			if is_empty then
				put_front (v)
			elseif after then
				back
				put_right (v)
				move (2)
			else
				temp := item
				replace (v)
				put_right (temp)
				forth
			end
		ensure
			sequence_effect: sequence |=| old sequence.extended_at (index, v)
			index_effect: index = old index + 1
		end

	put_right (v: like item)
			-- Add `v' to the right of cursor position.
			-- Do not move cursor.
		note
			modify: sequence
		require
			not_after: not after
		deferred
		ensure
			sequence_effect: sequence |=| old sequence.extended_at (index + 1, v)
		end

	replace (v: G)
			-- Replace current item by `v'.
			-- (from ACTIVE)
		note
			modify: sequence
		require
			writable: writable
		deferred
		ensure
			sequence_effect: sequence |=| old (sequence.replaced_at (index, v))
		end

feature -- Removal

	prune (v: like item)
			-- Remove first occurrence of `v', if any,
			-- after cursor position.
			-- If found, move cursor to right neighbor;
			-- if not, make structure exhausted.
			-- (from DYNAMIC_CHAIN)
		note
			modify: sequence, index
		require
			prunable: prunable
		do
			search (v)
			if not exhausted then
				remove
			end
		ensure
			sequence_effect_has: old sequence.tail (index).range.exists (agent element_equal (v, ?)) implies
				sequence |=| old sequence.removed_at (index - 1 + first_that (sequence.tail (index), agent element_equal (v, ?)))
			sequence_effect_not_has: not old sequence.tail (index).range.exists (agent element_equal (v, ?)) implies
				sequence |=| old sequence
			index_effect_has: old sequence.tail (index).range.exists (agent element_equal (v, ?)) implies
				index = old (index - 1 + first_that (sequence.tail (index), agent element_equal (v, ?)))
			index_effect_not_has: not old sequence.tail (index).range.exists (agent element_equal (v, ?)) implies
				index = sequence.count + 1
		end

	prune_all (v: like item)
			-- Remove all occurrences of `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- Leave structure exhausted.
			-- (from DYNAMIC_CHAIN)
		note
			modify: sequence, index
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
		ensure
			sequence_effect: sequence |=| old (sequence.removed (sequence.inverse.restricted (sequence.range | agent element_equal (v, ?)).range))
			index_effect: index = sequence.count + 1
		end

	remove
			-- Remove current item.
			-- Move cursor to right neighbor
			-- (or after if no right neighbor).
		note
			modify: sequence
		require -- from ACTIVE
			writable: writable
		deferred
		ensure
			sequence_effect: sequence |=| old sequence.removed_at (index)
		end

	remove_left
			-- Remove item to the left of cursor position.
			-- Do not move cursor.
		note
			modify: sequence, index
		require
			left_exists: index > 1
		deferred
		ensure
			sequence_effect: sequence |=| old sequence.removed_at (index - 1)
			index_effect: index = old index - 1
		end

	remove_right
			-- Remove item to the right of cursor position.
			-- Do not move cursor.
		note
			modify: sequence
		require
			right_exists: index < count
		deferred
		ensure
			sequence_effect: sequence |=| old sequence.removed_at (index + 1)
		end

	wipe_out
			-- Remove all items.
		note
			modify: sequence, index
		do
			chain_wipe_out
			back
		ensure
			sequence_effect: sequence.is_empty
			index_effect: index = 0
		end

feature {NONE} -- Removal

	chain_wipe_out
			-- Remove all items.
			-- (from DYNAMIC_CHAIN)
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

	swap (i: INTEGER_32)
			-- Exchange item at `i'-th position with item
			-- at cursor position.
			-- (from CHAIN)
		note
			modify: sequence
		require -- from CHAIN
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

	linear_representation: FLAT_DYNAMIC_LIST [G]
			-- Representation as a linear structure
			-- (from LINEAR)
		do
			Result := Current
		ensure
			definition: Result = Current
		end

feature -- Duplication

	duplicate (n: INTEGER_32): like Current
			-- Copy of sub-chain beginning at current position
			-- and having min (`n', `from_here') items,
			-- where `from_here' is the number of items
			-- at or to the right of current position.
			-- (from DYNAMIC_CHAIN)
		require -- from CHAIN
			not_off_unless_after: off implies after
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
				pos := cursor
			until
				(counter = n) or else exhausted
			loop
				Result.extend (item)
				forth
				counter := counter + 1
			end
			go_to (pos)
		ensure
			sequence_definition: Result.sequence |=| sequence.interval (index, index + n - 1)
			index_definition: Result.index = 0
			object_comparison_definition: Result.object_comparison = object_comparison
		end

feature {DYNAMIC_CHAIN} -- Implementation

	new_chain: FLAT_DYNAMIC_LIST [G]
			-- A newly created instance of the same type.
			-- This feature may be redefined in descendants so as to
			-- produce an adequately allocated and initialized object.
			-- (from DYNAMIC_CHAIN)
		deferred
		end

feature -- Iteration

	do_all (action: PROCEDURE [ANY, TUPLE [G]])
			-- Apply `action' to every item.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		note
			modify: sequence
		require -- from TRAVERSABLE
			action_exists: action /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_DYNAMIC_LIST [G]
		do
			if attached {FLAT_DYNAMIC_LIST [G]} Current as acs then
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
		ensure
			-- ToDo: postcondition is not implemented
--			sequence_effect: sequence.range.for_all (agent (a: PROCEDURE [ANY, TUPLE [G]]; x: G): BOOLEAN
--				do
--					Result := a.postcondition ([x])
--				end (action, ?))
		end

	do_if (action: PROCEDURE [ANY, TUPLE [G]]; test: FUNCTION [ANY, TUPLE [G], BOOLEAN])
			-- Apply `action' to every item that satisfies `test'.
			-- Semantics not guaranteed if `action' or `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from LINEAR)
		note
			modify: sequence
		require -- from TRAVERSABLE
			action_exists: action /= Void
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_DYNAMIC_LIST [G]
		do
			if attached {FLAT_DYNAMIC_LIST [G]} Current as acs then
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
		ensure
			-- ToDo: postcondition is not implemented
--			sequence_effect: sequence.range.for_all (agent (a: PROCEDURE [ANY, TUPLE [G]]; t: FUNCTION [ANY, TUPLE [G], BOOLEAN]; x: G): BOOLEAN
--				do
--					Result := t.item ([x]) implies a.postcondition ([x])
--				end (action, test, ?))
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
			cs: FLAT_DYNAMIC_LIST [G]
		do
			if attached {FLAT_DYNAMIC_LIST [G]} Current as acs then
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
		require -- from TRAVERSABLE
			test_exists: test /= Void
		local
			c: FLAT_CURSOR
			cs: FLAT_DYNAMIC_LIST [G]
		do
			if attached {FLAT_DYNAMIC_LIST [G]} Current as acs then
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

feature -- Specification

	sequence: MML_SEQUENCE [G]
			-- Sequence of elements.
		note
			status: specification
		local
			c: like new_cursor
		do
			create Result
			from
				c := new_cursor
			until
				c.after
			loop
				Result := Result & c.item
				c.forth
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

	first_that (s: MML_SEQUENCE [G]; pred: PREDICATE [ANY, TUPLE [G]]): INTEGER
			-- Index of the first element of `s' satisfying `pred'.
		note
			status: specification
		require
			exists: s.range.exists (pred)
		do
			Result := s.inverse.restricted (s.range | pred).range.extremum (agent less_equal)
		ensure
			definition: Result = s.inverse.restricted (s.range | pred).range.extremum (agent less_equal)
		end

invariant

	map_domain_definition: map.domain |=| sequence.domain
	map_definition: map.domain.for_all (agent (i: INTEGER): BOOLEAN
		do
			Result := map [i] = sequence [i]
		end)
	lower_definition: lower = 1

	index_not_too_small: index >= 0
	index_not_too_large: index <= sequence.count + 1

end -- class FLAT_DYNAMIC_LIST

