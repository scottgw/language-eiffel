note
	model: sequence, index, object_comparison

class
	FLAT_ARRAYED_SET [G]

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
			lower, -- specification
			upper
		end

create
	make


create {FLAT_ARRAYED_SET}
	make_filled

feature {NONE} -- Initialization

	make (n: INTEGER_32)
			-- Allocate list with `n' items.
			-- (`n' may be zero for empty list.)
			-- (from ARRAYED_LIST)
		require
			valid_number_of_items: n >= 0
		do
			index := 0
			create area_v2.make_empty (n)
		ensure
			sequence_effect: sequence.is_empty
			index_effect: index = 0
			object_comparison_effect: not object_comparison
		end

	make_empty_area (n: INTEGER_32)
			-- Creates a special object for `n' entries.
			-- (from TO_SPECIAL)
		require
			non_negative_argument: n >= 0
		do
			create area_v2.make_empty (n)
		end

	make_filled (n: INTEGER_32)
			-- Allocate list with `n' items.
			-- (`n' may be zero for empty list.)
			-- This list will be full.
			-- (from ARRAYED_LIST)
		require
			valid_number_of_items: n >= 0
			has_default: ({G}).has_default
		do
			index := 0
			make_filled_area (({G}).default, n)
		end

	make_filled_area (a_default_value: G; n: INTEGER_32)
			-- Creates a special object for `n' entries.
			-- (from TO_SPECIAL)
		require
			non_negative_argument: n >= 0
		do
			create area_v2.make_filled (a_default_value, n)
		end

	make_from_array (a: FLAT_ARRAY [G])
			-- Create list from array `a'.
			-- (from ARRAYED_LIST)
		require
			array_exists: a /= Void
		do
			index := 0
			area_v2 := a.area
		end

feature -- Access

	has (v: like item): BOOLEAN
			-- Does current include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from ARRAYED_LIST)
		local
			l_area: like area_v2
			i, nb: INTEGER_32
		do
			l_area := area_v2
			nb := count - 1
			if object_comparison and v /= Void then
				from
				until
					i > nb or Result
				loop
					Result := v ~ l_area.item (i)
					i := i + 1
				end
			else
				from
				until
					i > nb or Result
				loop
					Result := v = l_area.item (i)
					i := i + 1
				end
			end
		end

	index: INTEGER_32
			-- Index of item, if valid.
			-- (from ARRAYED_LIST)

	item: G
			-- Current item
			-- (from ARRAYED_LIST)
		do
			Result := area_v2.item (index - 1)
		end

	to_array: FLAT_ARRAY [G]
			-- Share content to be used as an ARRAY.
			-- Note that although the content is shared, it might
			-- not be shared when a resizing occur in either ARRAY or Current.
			-- (from ARRAYED_LIST)
		do
			create Result.make_from_special (area_v2)
		ensure
			map_definition: Result.map |=| map
		end

feature {FLAT_ARRAYED_SET} -- Access

	area: SPECIAL [G]
			-- Access to internal storage of ARRAYED_LIST
			-- (from ARRAYED_LIST)
		do
			Result := area_v2
		end

	area_v2: SPECIAL [G]
			-- Special data zone
			-- (from TO_SPECIAL)

	cursor: FLAT_ARRAYED_LIST_CURSOR
			-- Current cursor position
			-- (from ARRAYED_LIST)
		do
			create Result.make (index)
		end

	i_th alias "[]" (i: INTEGER_32): like item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in ARRAYED_LIST as synonym of at.
			-- (from ARRAYED_LIST)
		do
			Result := area_v2.item (i - 1)
		end

feature {NONE} -- Access

	at alias "@" (i: INTEGER_32): like item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in ARRAYED_LIST as synonym of i_th.
			-- (from ARRAYED_LIST)
		require
			valid_key: valid_index (i)
		do
			Result := area_v2.item (i - 1)
		end

	array_at (i: INTEGER_32): G assign array_put
			-- Entry at index `i', if in index interval
			-- Was declared in TO_SPECIAL as synonym of item.
			-- (from TO_SPECIAL)
		require
			valid_index: array_valid_index (i)
		do
			Result := area_v2.item (i)
		end

	first: like item
			-- Item at first position
			-- (from ARRAYED_LIST)
		require
			not_empty: not is_empty
		do
			Result := area_v2.item (0)
		end

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

	index_of (v: like item; i: INTEGER_32): INTEGER_32
			-- Index of `i'-th occurrence of item identical to `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- 0 if none.
			-- (from CHAIN)
		require
			positive_occurrences: i > 0
		local
			pos: FLAT_ARRAYED_LIST_CURSOR
		do
			pos := cursor
			Result := sequential_index_of (v, i)
			go_to (pos)
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

	array_item (i: INTEGER_32): G assign array_put
			-- Entry at index `i', if in index interval
			-- Was declared in TO_SPECIAL as synonym of at.
			-- (from TO_SPECIAL)
		require
			valid_index: array_valid_index (i)
		do
			Result := area_v2.item (i)
		end

	item_for_iteration: G
			-- Item at current position
			-- (from LINEAR)
		require
			not_off: not off
		do
			Result := item
		end

	last: like first
			-- Item at last position
			-- (from ARRAYED_LIST)
		require
			not_empty: not is_empty
		do
			Result := area_v2.item (count - 1)
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

feature {FLAT_ARRAYED_SET} -- Measurement

	Lower: INTEGER_32 = 1
			-- Lower bound for accessing list items via indexes
			-- (from ARRAYED_LIST)

	upper: INTEGER_32
			-- Upper bound for accessing list items via indexes
			-- (from ARRAYED_LIST)
		do
			Result := area_v2.count
		end

feature -- Measurement

	count: INTEGER_32
			-- Number of items
			-- (from ARRAYED_LIST)
		do
			Result := area_v2.count
		end

feature {NONE} -- Measurement

	additional_space: INTEGER_32
			-- Proposed number of additional items
			-- (from RESIZABLE)
		do
			Result := (capacity // 2).max (Minimal_increase)
		end

	capacity: INTEGER_32
			-- Number of items that may be stored
			-- (from ARRAYED_LIST)
		do
			Result := area_v2.capacity
		end

	Growth_percentage: INTEGER_32 = 50
			-- Percentage by which structure will grow automatically
			-- (from RESIZABLE)

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
			-- (from CHAIN)
		do
			create Result.make (1, count)
		end

	Minimal_increase: INTEGER_32 = 5
			-- Minimal number of additional items
			-- (from RESIZABLE)

	occurrences (v: like item): INTEGER_32
			-- Number of times `v' appears.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from CHAIN)
		local
			pos: FLAT_ARRAYED_LIST_CURSOR
		do
			pos := cursor
			Result := sequential_occurrences (v)
			go_to (pos)
		end

feature -- Comparison

	is_equal (other: FLAT_ARRAYED_SET [G]): BOOLEAN
			-- Is array made of the same items as `other'?
			-- (from ARRAYED_LIST)
		local
			i: INTEGER_32
		do
			if other = Current then
				Result := True
			elseif count = other.count and then object_comparison = other.object_comparison then
				if object_comparison then
					from
						Result := True
						i := Lower
					until
						not Result or i > upper
					loop
						Result := i_th (i) ~ other.i_th (i)
						i := i + 1
					end
				else
					Result := area_v2.same_items (other.area_v2, 0, 0, count)
				end
			end
		ensure then
			definition: Result = (object_comparison = other.object_comparison and
				sequence.count = other.sequence.count and
				sequence.domain.for_all (agent (j: INTEGER_32; o: FLAT_ARRAYED_SET [G]): BOOLEAN
				do
					Result := element_equal (o.sequence [j], sequence [j])
				end (?, other)))
		end

feature -- Status report

	after: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?
			-- (from LIST)
		do
			Result := (index = count + 1)
		end

	before: BOOLEAN
			-- Is there no valid cursor position to the left of cursor?
			-- (from LIST)
		do
			Result := (index = 0)
		end

	extendible: BOOLEAN
			-- May new items be added? (Answer: yes.)
			-- (from DYNAMIC_CHAIN)
		do
			Result := True
		end

	is_empty: BOOLEAN
			-- Is structure empty?
			-- (from FINITE)
		do
			Result := (count = 0)
		end

	islast: BOOLEAN
			-- Is cursor at last position?
			-- (from CHAIN)
		do
			Result := not is_empty and (index = count)
		end

	off: BOOLEAN
			-- Is there no current item?
			-- (from CHAIN)
		do
			Result := (index = 0) or (index = count + 1)
		end

	prunable: BOOLEAN
			-- May items be removed? (Answer: yes.)
			-- (from ARRAYED_LIST)
		do
			Result := True
		end

	readable: BOOLEAN
			-- Is there a current item that may be read?
			-- (from SEQUENCE)
		do
			Result := not off
		ensure
			definition: Result = sequence.domain [index]
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
			-- Is `i' a valid index?
			-- (from ARRAYED_LIST)
		do
			Result := (1 <= i) and (i <= count)
		end

	writable: BOOLEAN
			-- Is there a current item that may be modified?
			-- (from SEQUENCE)
		do
			Result := not off
		ensure
			definition: Result = sequence.domain [index]
		end

feature {FLAT_ARRAYED_SET} -- Status report

	full: BOOLEAN
			-- Is structure full?
			-- (from BOUNDED)
		do
			Result := (count = capacity)
		end

	valid_cursor (p: FLAT_CURSOR): BOOLEAN
			-- Can the cursor be moved to position `p'?
			-- (from ARRAYED_LIST)
		do
			if attached {FLAT_ARRAYED_LIST_CURSOR} p as al_c then
				Result := valid_cursor_index (al_c.index)
			end
		end

feature {NONE} -- Status report

	all_default: BOOLEAN
			-- Are all items set to default values?
			-- (from ARRAYED_LIST)
		require
			has_default: ({G}).has_default
		do
			Result := area_v2.filled_with (({G}).default, 0, area_v2.upper)
		end

	exhausted: BOOLEAN
			-- Has structure been completely explored?
			-- (from LINEAR)
		do
			Result := off
		end

	isfirst: BOOLEAN
			-- Is cursor at first position?
			-- (from CHAIN)
		do
			Result := not is_empty and (index = 1)
		end

	replaceable: BOOLEAN
			-- Can current item be replaced?
			-- (from ACTIVE)
		do
			Result := True
		end

	resizable: BOOLEAN
			-- May capacity be changed? (Answer: yes.)
			-- (from RESIZABLE)
		do
			Result := True
		end

	array_valid_index (i: INTEGER_32): BOOLEAN
			-- Is `i' within the bounds of Current?
			-- (from TO_SPECIAL)
		do
			Result := area_v2.valid_index (i)
		end

feature {NONE} -- Cursor movement

	back
			-- Move cursor one position backward.
			-- (from ARRAYED_LIST)
		require
			not_before: not before
		do
			index := index - 1
		end

	finish
			-- Move cursor to last position if any.
			-- (from ARRAYED_LIST)
		do
			index := count
		end

	move (i: INTEGER_32)
			-- Move cursor `i' positions.
			-- (from ARRAYED_LIST)
		do
			index := index + i
			if (index > count + 1) then
				index := count + 1
			elseif (index < 0) then
				index := 0
			end
		end

	search (v: like item)
			-- Move to first position (at or after current
			-- position) where item and `v' are equal.
			-- If structure does not include `v' ensure that
			-- exhausted will be true.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from ARRAYED_LIST)
		local
			l_area: like area_v2
			i, nb: INTEGER_32
			l_found: BOOLEAN
		do
			l_area := area_v2
			nb := count - 1
			i := (index - 1).max (0)
			if object_comparison and v /= Void then
				from
				until
					i > nb or l_found
				loop
					l_found := v ~ l_area.item (i)
					i := i + 1
				end
			else
				from
				until
					i > nb or l_found
				loop
					l_found := v = l_area.item (i)
					i := i + 1
				end
			end
			if l_found then
				index := i
			else
				index := i + 1
			end
		end

feature -- Cursor movement

	forth
			-- Move cursor one position forward.
			-- (from ARRAYED_LIST)
		do
			index := index + 1
		end

	go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th position.
			-- (from ARRAYED_LIST)
		require else
			valid_cursor_index: valid_cursor_index (i)
		do
			index := i
		end

	start
			-- Move cursor to first position if any.
			-- (from ARRAYED_LIST)
		do
			index := 1
		end

feature {FLAT_ARRAYED_SET} -- Cursor movement

	go_to (p: FLAT_CURSOR)
			-- Move cursor to position `p'.
			-- (from ARRAYED_LIST)
		require
			cursor_position_valid: valid_cursor (p)
		do
			if attached {FLAT_ARRAYED_LIST_CURSOR} p as al_c then
				index := al_c.index
			else
				check
					correct_cursor_type: False
				end
			end
		end

feature -- Element change

	extend (v: G)
			-- Insert `v' if not present.
			-- Was declared in FLAT_ARRAYED_SET as synonym of put.
		do
			if is_empty or else not has (v) then
				al_extend (v)
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

	put (v: G)
			-- Insert `v' if not present.
			-- Was declared in FLAT_ARRAYED_SET as synonym of extend.
		do
			if is_empty or else not has (v) then
				al_extend (v)
			end
		end

	put_left (v: like item)
			-- Add `v' to the left of current position.
			-- Do not move cursor.
			-- (from ARRAYED_LIST)
		do
			if after or is_empty then
				extend (v)
			else
				insert (v, index)
			end
			index := index + 1
		end

feature {NONE} -- Element change

	append (s: FLAT_ARRAYED_LIST [G])
			-- Append a copy of `s'.
			-- (from ARRAYED_LIST)
		require -- from SEQUENCE
			argument_not_void: s /= Void
		local
			c, old_count, new_count: INTEGER_32
		do
			c := s.count
			if c > 0 then
				old_count := count
				new_count := old_count + s.count
				if new_count > area_v2.capacity then
					area_v2 := area_v2.aliased_resized_area (new_count)
				end
				area_v2.copy_data (s.area_v2, 0, old_count, c)
			end
		end

	al_extend (v: like item)
			-- Add `v' to end.
			-- Do not move cursor.
			-- (from ARRAYED_LIST)
		require -- from COLLECTION
			extendible: extendible
		local
			i: INTEGER_32
			l_area: like area_v2
		do
			i := count + 1
			l_area := area_v2
			if i > l_area.capacity then
				l_area := l_area.aliased_resized_area (i + additional_space)
				area_v2 := l_area
			end
			l_area.extend (v)
		end

	force (v: like item)
			-- Add `v' to end.
			-- Do not move cursor.
			-- Was declared in ARRAYED_LIST as synonym of extend.
			-- (from ARRAYED_LIST)
		require -- from SEQUENCE
			extendible: extendible
		local
			i: INTEGER_32
			l_area: like area_v2
		do
			i := count + 1
			l_area := area_v2
			if i > l_area.capacity then
				l_area := l_area.aliased_resized_area (i + additional_space)
				area_v2 := l_area
			end
			l_area.extend (v)
		end

	merge_left (other: FLAT_ARRAYED_SET [G])
			-- Merge `other' into current structure before cursor.
			-- (from ARRAYED_LIST)
		require
			extendible: extendible
			not_before: not before
			other_exists: other /= Void
			not_current: other /= Current
		local
			old_index: INTEGER_32
			old_other_count: INTEGER_32
		do
			old_index := index
			old_other_count := other.count
			index := index - 1
			merge_right (other)
			index := old_index + old_other_count
		end

	merge_right (other: FLAT_ARRAYED_SET [G])
			-- Merge `other' into current structure after cursor.
			-- (from ARRAYED_LIST)
		require
			extendible: extendible
			not_after: not after
			other_exists: other /= Void
			not_current: other /= Current
		local
			l_new_count, l_old_count: INTEGER_32
		do
			if not other.is_empty then
				l_old_count := count
				l_new_count := l_old_count + other.count
				if l_new_count > area_v2.capacity then
					area_v2 := area_v2.aliased_resized_area (l_new_count)
				end
				area_v2.insert_data (other.area_v2, 0, index, other.count)
				other.wipe_out
			end
		end

	al_put (v: like item)
			-- Replace current item by `v'.
			-- (Synonym for replace)
			-- (from CHAIN)
		require
			writeable: writable
			replaceable: replaceable
		do
			replace (v)
		end

	array_put (v: G; i: INTEGER_32)
			-- Replace `i'-th entry, if in index interval, by `v'.
			-- (from TO_SPECIAL)
		require
			valid_index: array_valid_index (i)
		do
			area_v2.put (v, i)
		end

	sequence_put (v: like item)
			-- Add `v' to end.
			-- (from SEQUENCE)
		require
			extendible: extendible
		do
			extend (v)
		end

	put_front (v: like item)
			-- Add `v' to the beginning.
			-- Do not move cursor.
			-- (from ARRAYED_LIST)
		require
			extendible: extendible
		do
			if is_empty then
				extend (v)
			else
				insert (v, 1)
			end
			index := index + 1
		end

	put_i_th (v: like i_th; i: INTEGER_32)
			-- Replace `i'-th entry, if in index interval, by `v'.
			-- (from ARRAYED_LIST)
		require
			valid_key: valid_index (i)
		do
			area_v2.put (v, i - 1)
		end

	put_right (v: like item)
			-- Add `v' to the right of current position.
			-- Do not move cursor.
			-- (from ARRAYED_LIST)
		require
			extendible: extendible
			not_after: not after
		do
			if index = count then
				extend (v)
			else
				insert (v, index + 1)
			end
		end

	replace (v: like first)
			-- Replace current item by `v'.
			-- (from ARRAYED_LIST)
		require
			writable: writable
			replaceable: replaceable
		do
			put_i_th (v, index)
		end

	set_area (other: like area_v2)
			-- Make `other' the new area
			-- (from TO_SPECIAL)
		do
			area_v2 := other
		end

feature -- Removal

	prune (v: like item)
			-- Remove `v' if present.
		do
			start
			al_prune (v)
		end

	prune_all (v: like item)
			-- Remove all occurrences of `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from ARRAYED_LIST)
		local
			i, nb: INTEGER_32
			offset: INTEGER_32
			res: BOOLEAN
			obj_cmp: BOOLEAN
			l_area: like area_v2
		do
			obj_cmp := object_comparison
			from
				l_area := area_v2
				i := 0
				nb := count
			until
				i = count
			loop
				if i < nb - offset then
					if offset > 0 then
						l_area.put (l_area.item (i + offset), i)
					end
					if obj_cmp then
						res := v ~ l_area.item (i)
					else
						res := v = l_area.item (i)
					end
					if res then
						offset := offset + 1
					else
						i := i + 1
					end
				else
					i := i + 1
				end
			end
			l_area.remove_tail (offset)
			index := count + 1
		ensure then
			index_effect: index = sequence.count + 1
		end

	remove
			-- Remove current item.
			-- Move cursor to right neighbor
			-- (or after if no right neighbor)
			-- (from ARRAYED_LIST)
		do
			if index < count then
				area_v2.move_data (index, index - 1, count - index)
			end
			area_v2.remove_tail (1)
		end

	wipe_out
			-- Remove all items.
			-- (from ARRAYED_LIST)
		do
			area_v2.wipe_out
			index := 0
		end

feature {NONE} -- Removal

	al_prune (v: like item)
			-- Remove first occurrence of `v', if any,
			-- after cursor position.
			-- Move cursor to right neighbor.
			-- (or after if no right neighbor or `v' does not occur)
			-- (from ARRAYED_LIST)
		require -- from COLLECTION
			prunable: prunable
		do
			if before then
				index := 1
			end
			if object_comparison then
				from
				until
					after or else item ~ v
				loop
					forth
				end
			else
				from
				until
					after or else item = v
				loop
					forth
				end
			end
			if not after then
				remove
			end
		end

	remove_left
			-- Remove item to the left of cursor position.
			-- Do not move cursor.
			-- (from ARRAYED_LIST)
		require
			left_exists: index > 1
		do
			index := index - 1
			remove
		end

	remove_right
			-- Remove item to the right of cursor position
			-- Do not move cursor
			-- (from ARRAYED_LIST)
		require
			right_exists: index < count
		do
			index := index + 1
			remove
			index := index - 1
		end

	chain_wipe_out
			-- Remove all items.
			-- (from DYNAMIC_CHAIN)
		require
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

feature {NONE} -- Resizing

	automatic_grow
			-- Change the capacity to accommodate at least
			-- Growth_percentage more items.
			-- (from RESIZABLE)
		require
			resizable: resizable
		do
			grow (capacity + additional_space)
		end

	grow (i: INTEGER_32)
			-- Change the capacity to at least `i'.
			-- (from ARRAYED_LIST)
		require
			resizable: resizable
		do
			if i > area_v2.capacity then
				area_v2 := area_v2.aliased_resized_area (i)
			end
		end

	resize (new_capacity: INTEGER_32)
			-- Resize list so that it can contain
			-- at least `n' items. Do not lose any item.
			-- (from ARRAYED_LIST)
		require
			resizable: resizable
			new_capacity_large_enough: new_capacity >= capacity
		do
			grow (new_capacity)
		end

	trim
			-- Decrease capacity to the minimum value.
			-- Apply to reduce allocated storage.
			-- (from ARRAYED_LIST)
		local
			n: like count
		do
			n := count
			if n < area_v2.capacity then
				area_v2 := area_v2.aliased_resized_area (n)
			end
		end

feature {NONE} -- Transformation

	swap (i: INTEGER_32)
			-- Exchange item at `i'-th position with item
			-- at cursor position.
			-- (from ARRAYED_LIST)
		require
			not_off: not off
			valid_index: valid_index (i)
		local
			old_item: like item
		do
			old_item := item
			replace (area_v2.item (i - 1))
			area_v2.put (old_item, i - 1)
		end

feature -- Conversion

	linear_representation: FLAT_ARRAYED_SET [G]
			-- Representation as a linear structure
			-- (from LINEAR)
		do
			Result := Current
		end

feature -- Duplication

	copy (other: FLAT_ARRAYED_SET [G])
			-- Reinitialize by copying all the items of `other'.
			-- (This is also used by clone.)
			-- (from ARRAYED_LIST)
		note
			modify: sequence, index, object_comparison
		do
			if other /= Current then
				standard_copy (other)
				set_area (other.area_v2.twin)
			end
		ensure then
			sequence_effect: sequence |=| other.sequence
			index_effect: index = other.index
			object_comparison_effect: object_comparison = other.object_comparison
		end

	duplicate (n: INTEGER_32): FLAT_ARRAYED_SET [G]
			-- Copy of sub-list beginning at current position
			-- and having min (`n', count - index + 1) items.
			-- (from ARRAYED_LIST)
--		require -- from CHAIN
--			not_off_unless_after: off implies after
--			valid_subchain: n >= 0
		local
			end_pos: INTEGER_32
		do
			if after then
				Result := new_filled_list (0)
			else
				end_pos := count.min (index + n - 1)
				Result := new_filled_list (end_pos - index + 1)
				Result.area_v2.copy_data (area_v2, index - 1, 0, end_pos - index + 1)
			end
		end

feature {NONE} -- Inapplicable

	new_chain: FLAT_ARRAYED_SET [G]
			-- Unused
			-- (from ARRAYED_LIST)
		do
			Result := Current
		end

feature {NONE} -- Implementation

	force_i_th (v: like i_th; pos: INTEGER_32)
			-- (from ARRAYED_LIST)
		do
			if count + 1 > capacity then
				grow (count + additional_space)
			end
			area_v2.force (v, pos)
		end

	insert (v: like item; pos: INTEGER_32)
			-- Add `v' at `pos', moving subsequent items
			-- to the right.
			-- (from ARRAYED_LIST)
		require
			index_small_enough: pos <= count
			index_large_enough: pos >= 1
		do
			if count + 1 > capacity then
				grow (count + additional_space)
			end
			area_v2.move_data (pos - 1, pos, count - pos + 1)
			put_i_th (v, pos)
		end

	new_filled_list (n: INTEGER_32): FLAT_ARRAYED_SET [G]
			-- New list with `n' elements.
			-- (from ARRAYED_LIST)
		require
			n_non_negative: n >= 0
		do
			create Result.make (n)
		end

feature {NONE} -- Iteration

	do_all (action: PROCEDURE [ANY, TUPLE [G]])
			-- Apply `action' to every item, from first to last.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from ARRAYED_LIST)
		require
			action_exists: action /= Void
		do
			area_v2.do_all_in_bounds (action, 0, area_v2.count - 1)
		end

	do_all_with_index (action: PROCEDURE [ANY, TUPLE [G, INTEGER_32]])
			-- Apply `action' to every item, from first to last.
			-- `action' receives item and its index.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from ARRAYED_LIST)
		require
			action_not_void: action /= Void
		local
			i, j, nb: INTEGER_32
			l_area: like area_v2
		do
			from
				i := 0
				j := Lower
				nb := count - 1
				l_area := area_v2
			until
				i > nb
			loop
				action.call ([l_area.item (i), j])
				j := j + 1
				i := i + 1
			end
		end

	do_if (action: PROCEDURE [ANY, TUPLE [G]]; test: FUNCTION [ANY, TUPLE [G], BOOLEAN])
			-- Apply `action' to every item that satisfies `test', from first to last.
			-- Semantics not guaranteed if `action' or `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from ARRAYED_LIST)
		require
			action_exists: action /= Void
			test_exists: test /= Void
		do
			area_v2.do_if_in_bounds (action, test, 0, area_v2.count - 1)
		end

	do_if_with_index (action: PROCEDURE [ANY, TUPLE [G, INTEGER_32]]; test: FUNCTION [ANY, TUPLE [G, INTEGER_32], BOOLEAN])
			-- Apply `action' to every item that satisfies `test', from first to last.
			-- `action' and `test' receive the item and its index.
			-- Semantics not guaranteed if `action' or `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
			-- (from ARRAYED_LIST)
		require
			action_not_void: action /= Void
			test_not_void: test /= Void
		local
			i, j, nb: INTEGER_32
			l_area: like area_v2
		do
			from
				i := 0
				j := Lower
				nb := count - 1
				l_area := area_v2
			until
				i > nb
			loop
				if test.item ([l_area.item (i), j]) then
					action.call ([l_area.item (i), j])
				end
				j := j + 1
				i := i + 1
			end
		end

	for_all (test: FUNCTION [ANY, TUPLE [G], BOOLEAN]): BOOLEAN
			-- Is `test' true for all items?
			-- (from ARRAYED_LIST)
		require
			test_exists: test /= Void
		do
			Result := area_v2.for_all_in_bounds (test, 0, area_v2.count - 1)
		end

	there_exists (test: FUNCTION [ANY, TUPLE [G], BOOLEAN]): BOOLEAN
			-- Is `test' true for at least one item?
			-- (from ARRAYED_LIST)
		require
			test_exists: test /= Void
		do
			Result := area_v2.there_exists_in_bounds (test, 0, area_v2.count - 1)
		end

invariant

	map_domain_definition: map.domain |=| sequence.domain
	map_definition: map.domain.for_all (agent (i: INTEGER): BOOLEAN
		do
			Result := map [i] = sequence [i]
		end)
	lower_definition: lower = 1

note
	library: "EiffelBase: Library of reusable components for Eiffel."
	copyright: "Copyright (c) 1984-2006, Eiffel Software and others"
	license: "Eiffel Forum License v2 (see http://www.eiffel.com/licensing/forum.txt)"
	source: "[
		Eiffel Software
		356 Storke Road, Goleta, CA 93117 USA
		Telephone 805-685-1006, Fax 805-685-6869
		Website http://www.eiffel.com
		Customer support http://support.eiffel.com
	]"

end -- class FLAT_ARRAYED_SET

