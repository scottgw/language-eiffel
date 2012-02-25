note
	model: sequence, index, capacity, object_comparison

class
	FLAT_ARRAYED_LIST [G]

inherit
	FLAT_DYNAMIC_LIST [G]
		redefine
			at,
			first,
			has,
			i_th,
			is_equal,
			last,
			is_inserted,
			copy,
			finish,
			go_i_th,
			move,
			search,
			start,
			append,
			force,
			merge_left,
			merge_right,
			put_i_th,
			put_left,
			prune,
			prune_all,
			wipe_out,
			swap,
			duplicate,
			do_all, do_if, for_all, there_exists,
			lower, -- specification
			upper
		end

create
	make,
	make_filled,
	make_from_array

feature -- Initialization

	make (n: INTEGER_32)
			-- Allocate list with `n' items.
			-- (`n' may be zero for empty list.)
		require
			valid_number_of_items: n >= 0
		do
			index := 0
			create area_v2.make_empty (n)
		ensure
			sequence_effect: sequence.is_empty
			index_effect: index = 0
			capacity_effect: capacity = n
			object_comparison_effect: not object_comparison
		end

	make_filled (n: INTEGER_32)
			-- Allocate list with `n' items.
			-- (`n' may be zero for empty list.)
			-- This list will be full.
		require
			valid_number_of_items: n >= 0
			has_default: ({G}).has_default
		do
			index := 0
			make_filled_area (({G}).default, n)
		ensure
			sequence_domain_effect: sequence.count = n
			sequence_effect: sequence.is_constant (({G}).default)
			index_effect: index = 0
			capacity_effect: capacity = n
			object_comparison_effect: not object_comparison
		end

	make_from_array (a: FLAT_ARRAY [G])
			-- Create list from array `a'.
		require
			array_exists: a /= Void
		do
			index := 0
			area_v2 := a.area
		ensure
			sequence_domain_effect: sequence.count = a.map.count
			sequence_effect: sequence.domain.for_all (agent (i: INTEGER; ar: FLAT_ARRAY [G]): BOOLEAN
				do
					Result := sequence [i] = ar.map [ar.lower + i - 1]
				end (?, a))
			index_effect: index = 0
			capacity_effect: capacity = a.map.count
			object_comparison_effect: not object_comparison
		end

feature {NONE} -- Initialization

	make_empty_area (n: INTEGER_32)
			-- Creates a special object for `n' entries.
			-- (from TO_SPECIAL)
		require
			non_negative_argument: n >= 0
		do
			create area_v2.make_empty (n)
		end

	make_filled_area (a_default_value: G; n: INTEGER_32)
			-- Creates a special object for `n' entries.
			-- (from TO_SPECIAL)
		require
			non_negative_argument: n >= 0
		do
			create area_v2.make_filled (a_default_value, n)
		end

feature -- Access

	array_at (i: INTEGER_32): G assign array_put
			-- Entry at index `i', if in index interval
			-- Was declared in TO_SPECIAL as synonym of item.
			-- (from TO_SPECIAL)
		require
			valid_index: array_valid_index (i)
		do
			Result := area_v2.item (i)
		ensure
			definition: Result = sequence [i + 1]
		end

	at alias "@" (i: INTEGER_32): like item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in FLAT_ARRAYED_LIST as synonym of i_th.
		do
			Result := area_v2.item (i - 1)
		end

	cursor: FLAT_ARRAYED_LIST_CURSOR
			-- Current cursor position
		do
			create Result.make (index)
		end

	first: like item
			-- Item at first position
		do
			Result := area_v2.item (0)
		end

	has (v: like item): BOOLEAN
			-- Does current include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
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

	i_th alias "[]" (i: INTEGER_32): like item assign put_i_th
			-- Item at `i'-th position
			-- Was declared in FLAT_ARRAYED_LIST as synonym of at.
		do
			Result := area_v2.item (i - 1)
		end

	index: INTEGER_32
			-- Index of item, if valid.

	array_item (i: INTEGER_32): G assign array_put
			-- Entry at index `i', if in index interval
			-- Was declared in TO_SPECIAL as synonym of at.
			-- (from TO_SPECIAL)
		require
			valid_index: array_valid_index (i)
		do
			Result := area_v2.item (i)
		ensure
			definition: Result = sequence [i + 1]
		end

	item: G
			-- Current item
		do
			Result := area_v2.item (index - 1)
		end

	last: like first
			-- Item at last position
		do
			Result := area_v2.item (count - 1)
		end

	to_array: FLAT_ARRAY [G]
			-- Share content to be used as an ARRAY.
			-- Note that although the content is shared, it might
			-- not be shared when a resizing occur in either ARRAY or Current.
		do
			create Result.make_from_special (area_v2)
		ensure
			map_definition: Result.map |=| map
		end

feature -- Measurement

	capacity: INTEGER_32
			-- Number of items that may be stored
		do
			Result := area_v2.capacity
		end

	count: INTEGER_32
			-- Number of items
		do
			Result := area_v2.count
		end

	Growth_percentage: INTEGER_32 = 50
			-- Percentage by which structure will grow automatically
			-- (from RESIZABLE)

	Lower: INTEGER_32 = 1
			-- Lower bound for accessing list items via indexes

	Minimal_increase: INTEGER_32 = 5
			-- Minimal number of additional items
			-- (from RESIZABLE)

	upper: INTEGER_32
			-- Upper bound for accessing list items via indexes
		do
			Result := area_v2.count
		end

feature -- Comparison

	is_equal (other: like Current): BOOLEAN
			-- Is array made of the same items as `other'?
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
		end

feature -- Status report

	all_default: BOOLEAN
			-- Are all items set to default values?
		require
			has_default: ({G}).has_default
		do
			Result := area_v2.filled_with (({G}).default, 0, area_v2.upper)
		ensure
			definition: Result = sequence.is_constant (({G}).default)
		end

	full: BOOLEAN
			-- Is structure full?
			-- (from BOUNDED)
		do
			Result := (count = capacity)
		ensure
			definition: Result = (sequence.count = capacity)
		end

	is_inserted (v: G): BOOLEAN
			-- Has `v' been inserted at the end by the most recent put or
			-- extend?
		do
			if not is_empty then
				Result := (v = last) or else (not off and then (v = item))
			end
		end

	resizable: BOOLEAN
			-- May capacity be changed? (Answer: yes.)
			-- (from RESIZABLE)
		do
			Result := True
		ensure
			definition: Result
		end

	valid_cursor (p: FLAT_CURSOR): BOOLEAN
			-- Can the cursor be moved to position `p'?
		do
			if attached {FLAT_ARRAYED_LIST_CURSOR} p as al_c then
				Result := valid_cursor_index (al_c.index)
			end
		end

	array_valid_index (i: INTEGER_32): BOOLEAN
			-- Is `i' within the bounds of Current?
			-- (from TO_SPECIAL)
		do
			Result := area_v2.valid_index (i)
		ensure
			definition: Result = (0 <= i and i < map.count)
		end

feature -- Cursor movement

	back
			-- Move cursor one position backward.
		do
			index := index - 1
		end

	finish
			-- Move cursor to last position if any.
		do
			index := count
		end

	forth
			-- Move cursor one position forward.
		do
			index := index + 1
		end

	go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th position.
		do
			index := i
		end

	go_to (p: FLAT_ARRAYED_LIST_CURSOR)
			-- Move cursor to position `p'.
		do
			if attached {FLAT_ARRAYED_LIST_CURSOR} p as al_c then
				index := al_c.index
			else
				check
					correct_cursor_type: False
				end
			end
		end

	move (i: INTEGER_32)
			-- Move cursor `i' positions.
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

	start
			-- Move cursor to first position if any.
		do
			index := 1
		end

feature -- Element change

	append (s: FLAT_DYNAMIC_LIST [G])
			-- Append a copy of `s'.
		local
			c, old_count, new_count: INTEGER_32
		do
			if attached {FLAT_ARRAYED_LIST [G]} s as al then -- Optimization for arrayed lists
				c := al.count
					-- If `s' is empty nothing to be done.
				if c > 0 then
					old_count := count
					new_count := old_count + al.count
					if new_count > area_v2.capacity then
						area_v2 := area_v2.aliased_resized_area (new_count)
					end
					area_v2.copy_data (al.area_v2, 0, old_count, c)
				end
			else
				Precursor {FLAT_DYNAMIC_LIST} (s)
			end
		end

	extend (v: like item)
			-- Add `v' to end.
			-- Do not move cursor.
			-- Was declared in FLAT_ARRAYED_LIST as synonym of force.
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
			-- Was declared in FLAT_ARRAYED_LIST as synonym of extend.
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

	merge_left (other: FLAT_ARRAYED_LIST [G])
			-- Merge `other' into current structure before cursor.
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

	merge_right (other: FLAT_ARRAYED_LIST [G])
			-- Merge `other' into current structure after cursor.
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

	array_put (v: G; i: INTEGER_32)
			-- Replace `i'-th entry, if in index interval, by `v'.
			-- (from TO_SPECIAL)
		note
			modify: sequence
		require
			valid_index: array_valid_index (i)
		do
			area_v2.put (v, i)
		ensure
			sequence_effect: sequence |=| old sequence.replaced_at (i + 1, v)
		end

	put_front (v: like item)
			-- Add `v' to the beginning.
			-- Do not move cursor.
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
		do
			area_v2.put (v, i - 1)
		end

	put_left (v: like item)
			-- Add `v' to the left of current position.
			-- Do not move cursor.
		do
			if after or is_empty then
				extend (v)
			else
				insert (v, index)
			end
			index := index + 1
		end

	put_right (v: like item)
			-- Add `v' to the right of current position.
			-- Do not move cursor.
		do
			if index = count then
				extend (v)
			else
				insert (v, index + 1)
			end
		end

	replace (v: like first)
			-- Replace current item by `v'.
		do
			put_i_th (v, index)
		end

feature {NONE} -- Element change

	set_area (other: like area_v2)
			-- Make `other' the new area
			-- (from TO_SPECIAL)
		do
			area_v2 := other
		end

feature -- Removal

	prune (v: like item)
			-- Remove first occurrence of `v', if any,
			-- after cursor position.
			-- Move cursor to right neighbor.
			-- (or after if no right neighbor or `v' does not occur)
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

	prune_all (v: like item)
			-- Remove all occurrences of `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
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
		end

	remove
			-- Remove current item.
			-- Move cursor to right neighbor
			-- (or after if no right neighbor)
		do
			if index < count then
				area_v2.move_data (index, index - 1, count - index)
			end
			area_v2.remove_tail (1)
		end

	remove_left
			-- Remove item to the left of cursor position.
			-- Do not move cursor.
		do
			index := index - 1
			remove
		end

	remove_right
			-- Remove item to the right of cursor position
			-- Do not move cursor
		do
			index := index + 1
			remove
			index := index - 1
		end

	wipe_out
			-- Remove all items.
		do
			area_v2.wipe_out
			index := 0
		end

feature -- Resizing

	automatic_grow
			-- Change the capacity to accommodate at least
			-- Growth_percentage more items.
			-- (from RESIZABLE)
		note
			modify: capacity
		do
			grow (capacity + additional_space)
		ensure
			capacity_effect: capacity > old capacity
		end

	grow (i: INTEGER_32)
			-- Change the capacity to at least `i'.
		note
			modify: capacity
		do
			if i > area_v2.capacity then
				area_v2 := area_v2.aliased_resized_area (i)
			end
		ensure
			capacity_effect: capacity >= i
		end

	resize (new_capacity: INTEGER_32)
			-- Resize list so that it can contain
			-- at least `n' items. Do not lose any item.
		note
			modify: capacity
		require
			resizable: resizable
			new_capacity_large_enough: new_capacity >= capacity
		do
			grow (new_capacity)
		ensure
			capacity_effect: capacity >= new_capacity
		end

	trim
			-- Decrease capacity to the minimum value.
			-- Apply to reduce allocated storage.
		note
			modify: capacity
		local
			n: like count
		do
			n := count
			if n < area_v2.capacity then
				area_v2 := area_v2.aliased_resized_area (n)
			end
		ensure
			capacity_effect: capacity = count
		end

feature -- Transformation

	swap (i: INTEGER_32)
			-- Exchange item at `i'-th position with item
			-- at cursor position.
		local
			old_item: like item
		do
			old_item := item
			replace (area_v2.item (i - 1))
			area_v2.put (old_item, i - 1)
		end

feature -- Duplication

	copy (other: like Current)
			-- Reinitialize by copying all the items of `other'.
			-- (This is also used by clone.)
		note
			modify: sequence, index, object_comparison, capacity
		do
			if other /= Current then
				standard_copy (other)
				set_area (other.area_v2.twin)
			end
		ensure then
			sequence_effect: sequence |=| other.sequence
			index_effect: index = other.index
			capacity_effect: capacity = other.capacity
			object_comparison_effect: object_comparison = other.object_comparison
		end

	duplicate (n: INTEGER_32): like Current
			-- Copy of sub-list beginning at current position
			-- and having min (`n', count - index + 1) items.
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

	new_chain: like Current
			-- Unused
		do
			Result := Current
		end

feature {FLAT_ARRAYED_LIST, FLAT_ARRAYED_SET} -- Implementation
	-- Moved here from various public feature clauses:

	area: SPECIAL [G]
			-- Access to internal storage of arrayed list
		do
			Result := area_v2
		end

	area_v2: SPECIAL [G]
			-- Special data zone
			-- (from TO_SPECIAL)

	additional_space: INTEGER_32
			-- Proposed number of additional items
			-- (from RESIZABLE)
		do
			Result := (capacity // 2).max (Minimal_increase)
		end

feature {NONE} -- Implementation

	force_i_th (v: like i_th; pos: INTEGER_32)
		do
			if count + 1 > capacity then
				grow (count + additional_space)
			end
			area_v2.force (v, pos)
		end

	insert (v: like item; pos: INTEGER_32)
			-- Add `v' at `pos', moving subsequent items
			-- to the right.
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

	new_filled_list (n: INTEGER_32): like Current
			-- New list with `n' elements.
		require
			n_non_negative: n >= 0
		do
			create Result.make (n)
		end

feature -- Iteration

	do_all (action: PROCEDURE [ANY, TUPLE [G]])
			-- Apply `action' to every item, from first to last.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
		do
			area_v2.do_all_in_bounds (action, 0, area_v2.count - 1)
		end

	do_all_with_index (action: PROCEDURE [ANY, TUPLE [G, INTEGER_32]])
			-- Apply `action' to every item, from first to last.
			-- `action' receives item and its index.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
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
		do
			area_v2.do_if_in_bounds (action, test, 0, area_v2.count - 1)
		end

	do_if_with_index (action: PROCEDURE [ANY, TUPLE [G, INTEGER_32]]; test: FUNCTION [ANY, TUPLE [G, INTEGER_32], BOOLEAN])
			-- Apply `action' to every item that satisfies `test', from first to last.
			-- `action' and `test' receive the item and its index.
			-- Semantics not guaranteed if `action' or `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
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

	for_all (test: PREDICATE [ANY, TUPLE [G]]): BOOLEAN
			-- Is `test' true for all items?
		do
			Result := area_v2.for_all_in_bounds (test, 0, area_v2.count - 1)
		end

	there_exists (test: PREDICATE [ANY, TUPLE [G]]): BOOLEAN
			-- Is `test' true for at least one item?
		do
			Result := area_v2.there_exists_in_bounds (test, 0, area_v2.count - 1)
		end

invariant

	sequence_and_capacity: sequence.count <= capacity

end -- class FLAT_ARRAYED_LIST

