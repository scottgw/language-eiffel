note
	model: map, lower, object_comparison

class
	FLAT_ARRAY [G]

inherit
	FLAT_READABLE_INDEXABLE [G]
		redefine
			copy,
			is_equal,
			lower, -- specification
			upper
		end

	V_EQUALITY [G]
		redefine
			copy,
			is_equal
		end

create
	make_empty,
	make,
	make_filled,
	make_from_array,
	make_from_special

convert
	to_special: {SPECIAL [G]}

feature -- Initialization

	make (min_index, max_index: INTEGER_32)
			-- Allocate array; set index interval to
			-- `min_index' .. `max_index'; set all values to default.
			-- (Make array empty if `min_index' = `max_index' + 1).
		require
			valid_bounds: min_index <= max_index + 1
			has_default: min_index <= max_index implies ({G}).has_default
		do
			lower := min_index
			upper := max_index
			if min_index <= max_index then
				make_filled_area (({G}).default, max_index - min_index + 1)
			else
				make_empty_area (0)
			end
		ensure
			map_domain_effect: map.domain |=| {MML_INTERVAL} [[min_index, max_index]]
			map_effect: map.is_constant (({G}).default)
			lower_effect: lower = min_index
			object_comparison_effect: not object_comparison
		end

	make_empty
			-- Allocate empty array starting at `1'.
		do
			lower := 1
			upper := 0
			make_empty_area (0)
		ensure
			map_effect: map.is_empty
			lower_effect: lower = 1
			object_comparison_effect: not object_comparison
		end

	make_filled (a_default_value: G; min_index, max_index: INTEGER_32)
			-- Allocate array; set index interval to
			-- `min_index' .. `max_index'; set all values to default.
			-- (Make array empty if `min_index' = `max_index' + 1).
		require
			valid_bounds: min_index <= max_index + 1
		local
			n: INTEGER_32
		do
			lower := min_index
			upper := max_index
			if min_index <= max_index then
				n := max_index - min_index + 1
			end
			make_filled_area (a_default_value, n)
		ensure
			map_domain_effect: map.domain |=| {MML_INTERVAL} [[min_index, max_index]]
			map_effect: map.is_constant (a_default_value)
			lower_effect: lower = min_index
			object_comparison_effect: not object_comparison
		end

	make_from_array (a: FLAT_ARRAY [G])
			-- Initialize from the items of `a'.
			-- (Useful in proper descendants of class `FLAT_ARRAY',
			-- to initialize an array-like object from a manifest array.)
		require
			array_exists: a /= Void
		do
			set_area (a.area)
			lower := a.lower
			upper := a.upper
		ensure
			map_effect: map |=| a.map
			lower_effect: lower = a.lower
			object_comparison_effect: not object_comparison
		end

	make_from_special (a: SPECIAL [G])
			-- Initialize Current from items of `a'.
		require
			special_attached: a /= Void
		do
			set_area (a)
			lower := 1
			upper := a.count
		ensure
			map_domain_effect: map.domain |=| {MML_INTERVAL} [[1, a.count]]
			map_effect: map.domain.for_all (agent (i: INTEGER; s: SPECIAL [G]): BOOLEAN
				do
					Result := map [i] = s [i - 1]
				end (?, a))
			lower_effect: lower = 1
			object_comparison_effect: not object_comparison
		end

feature {NONE} -- Initialization

	make_empty_area (n: INTEGER_32)
			-- Creates a special object for `n' entries.
			-- (from TO_SPECIAL)
		require
			non_negative_argument: n >= 0
		do
			create area.make_empty (n)
		end

	make_filled_area (a_default_value: G; n: INTEGER_32)
			-- Creates a special object for `n' entries.
			-- (from TO_SPECIAL)
		require
			non_negative_argument: n >= 0
		do
			create area.make_filled (a_default_value, n)
		end

feature -- Access

	at alias "@" (i: INTEGER_32): G assign put
			-- Entry at index `i', if in index interval
			-- Was declared in FLAT_ARRAY as synonym of item.
		require -- from TABLE
			valid_key: valid_index (i)
		do
			Result := area.item (i - lower)
		ensure
			definition: Result = map [i]
		end

	entry (i: INTEGER_32): G
			-- Entry at index `i', if in index interval
		require
			valid_key: valid_index (i)
		do
			Result := item (i)
		ensure
			definition: Result = map [i]
		end

	has (v: G): BOOLEAN
			-- Does `v' appear in array?
			-- (Reference or object equality,
			-- based on object_comparison.)
		local
			i, nb: INTEGER_32
			l_area: like area
		do
			l_area := area
			nb := upper - lower
			if object_comparison and v /= Void then
				from
				until
					i > nb or Result
				loop
					Result := l_area.item (i) ~ v
					i := i + 1
				end
			else
				from
				until
					i > nb or Result
				loop
					Result := l_area.item (i) = v
					i := i + 1
				end
			end
		ensure
			definition: Result = map.range.exists (agent element_equal (v, ?))
		end

	item alias "[]" (i: INTEGER_32): G assign put
			-- Entry at index `i', if in index interval
			-- Was declared in FLAT_ARRAY as synonym of at.
		do
			Result := area.item (i - lower)
		end

feature -- Measurement

	capacity: INTEGER_32
			-- Number of available indices
			-- Was declared in FLAT_ARRAY as synonym of count.
		do
			Result := upper - lower + 1
		ensure
			definition: Result = map.count
		end

	count: INTEGER_32
			-- Number of available indices
			-- Was declared in FLAT_ARRAY as synonym of capacity.
		do
			Result := upper - lower + 1
		ensure
			definition: Result = map.count
		end

	Growth_percentage: INTEGER_32 = 50
			-- Percentage by which structure will grow automatically
			-- (from RESIZABLE)

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
		do
			create Result.make (lower, upper)
		end

	lower: INTEGER_32
			-- Minimum index

	Minimal_increase: INTEGER_32 = 5
			-- Minimal number of additional items
			-- (from RESIZABLE)

	occurrences (v: G): INTEGER_32
			-- Number of times `v' appears in structure
		local
			i: INTEGER_32
		do
			if object_comparison then
				from
					i := lower
				until
					i > upper
				loop
					if item (i) ~ v then
						Result := Result + 1
					end
					i := i + 1
				end
			else
				from
					i := lower
				until
					i > upper
				loop
					if item (i) = v then
						Result := Result + 1
					end
					i := i + 1
				end
			end
		ensure
			definition: Result = occurrences_that (map, agent element_equal (v, ?))
		end

	upper: INTEGER_32
			-- Maximum index

feature -- Comparison

	is_equal (other: like Current): BOOLEAN
			-- Is array made of the same items as `other'?
		local
			i: INTEGER_32
		do
			if other = Current then
				Result := True
			elseif lower = other.lower and then upper = other.upper and then object_comparison = other.object_comparison then
				if object_comparison then
					from
						Result := True
						i := lower
					until
						not Result or i > upper
					loop
						Result := item (i) ~ other.item (i)
						i := i + 1
					end
				else
					Result := area.same_items (other.area, 0, 0, count)
				end
			end
		ensure then
			definition: Result = (lower = other.lower and
				object_comparison = other.object_comparison and
				map.count = other.map.count and
				map.domain.for_all (agent (j: INTEGER; o: FLAT_ARRAY [G]): BOOLEAN
					do
						Result := element_equal (o.map [j], map [j])
					end (?, other)))
		end

feature -- Status report

	all_default: BOOLEAN
			-- Are all items set to default values?
		do
			if count > 0 then
				Result := ({G}).has_default and then area.filled_with (({G}).default, 0, upper - lower)
			else
				Result := True
			end
		ensure
			definition: Result = map.is_constant (({G}).default)
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

	extendible: BOOLEAN
			-- May items be added?
			-- (Answer: no, although array may be resized.)
		do
			Result := False
		ensure
			definition: not Result
		end

	filled_with (v: G): BOOLEAN
			-- Are all itms set to `v'?
		do
			Result := area.filled_with (v, 0, upper - lower)
		ensure
			definition: Result = map.is_constant (v)
		end

	full: BOOLEAN
			-- Is structure filled to capacity? (Answer: yes)
		do
			Result := True
		ensure
			definition: Result
		end

	is_empty: BOOLEAN
			-- Is structure empty?
			-- (from FINITE)
		do
			Result := (count = 0)
		ensure
			definition: Result = map.is_empty
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
			definition: Result = map.range.exists (agent element_equal (v, ?))
		end

	object_comparison: BOOLEAN
			-- Must search operations use equal rather than `='
			-- for comparing references? (Default: no, use `='.)
			-- (from CONTAINER)

	prunable: BOOLEAN
			-- May items be removed? (Answer: no.)
		do
			Result := False
		ensure
			definition: not Result
		end

	resizable: BOOLEAN
			-- Can array be resized automatically?
		do
			Result := ({G}).has_default
		ensure
			definition: Result = ({G}).has_default
		end

	same_items (other: like Current): BOOLEAN
			-- Do `other' and Current have same items?
		require
			other_not_void: other /= Void
		do
			if count = other.count then
				Result := area.same_items (other.area, 0, 0, count)
			end
		ensure
			definition: Result = ((map.count = other.map.count) and
				({MML_INTERVAL} [[0, map.count - 1]]).for_all (agent (i: INTEGER; o: FLAT_ARRAY [G]): BOOLEAN
					do
						Result := o.map [o.lower + i] = map [lower + i]
					end (?, other)))
		end

	valid_index (i: INTEGER_32): BOOLEAN
			-- Is `i' within the bounds of the array?
		do
			Result := (lower <= i) and then (i <= upper)
		end

	valid_index_set: BOOLEAN
		do
			Result := index_set.count = count
		ensure
			definition: Result
		end

feature -- Status setting

	compare_objects
			-- Ensure that future search operations will use equal
			-- rather than `=' for comparing references.
			-- (from CONTAINER)
		note
			modify: object_comparison
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
		do
			object_comparison := False
		ensure
			object_comparison_effect: not object_comparison
		end

feature -- Element change

	enter (v: like item; i: INTEGER_32)
			-- Replace `i'-th entry, if in index interval, by `v'.
		note
			modify: map
		require
			valid_key: valid_index (i)
		do
			area.put (v, i - lower)
		ensure
			map_effect: map |=| old map.updated (i, v)
		end

	fill_with (v: G)
			-- Set items between lower and upper with `v'.
		note
			modify: map
		do
			area.fill_with (v, 0, upper - lower)
		ensure
			map_domain_unchanged: map.domain |=| old map.domain
			map_effect: map.is_constant (v)
		end

	force (v: like item; i: INTEGER_32)
			-- Assign item `v' to `i'-th entry.
			-- Resize the array if `i' falls out of currently defined bounds; preserve existing items.
			-- In void-safe mode, if ({G}).has_default does not hold, then you can only insert at either
			-- `lower - 1' or `upper + 1' position in the FLAT_ARRAY.
		note
			modify: map, lower
		require
			has_default: ({G}).has_default or else i = lower - 1 or else i = upper + 1
		local
			old_size, new_size: INTEGER_32
			new_lower, new_upper: INTEGER_32
			offset: INTEGER_32
			l_increased_by_one: BOOLEAN
		do
			new_lower := lower.min (i)
			new_upper := upper.max (i)
			new_size := new_upper - new_lower + 1
			l_increased_by_one := (i = upper + 1) or (i = lower - 1)
			if empty_area then
				make_empty_area (new_size.max (additional_space))
				if not l_increased_by_one then
					area.fill_with (({G}).default, 0, new_size - 2)
				end
				area.extend (v)
			else
				old_size := area.capacity
				if new_size > old_size then
					set_area (area.aliased_resized_area (new_size.max (old_size + additional_space)))
				end
				if new_lower < lower then
					offset := lower - new_lower
					area.move_data (0, offset, capacity)
					if not l_increased_by_one then
						area.fill_with (({G}).default, 1, offset - 2)
					end
					area.put (v, 0)
				else
					if new_size > area.count then
						if not l_increased_by_one then
							area.fill_with (({G}).default, area.count, new_size - 2)
						end
						area.extend (v)
					else
						area.put (v, i - lower)
					end
				end
			end
			lower := new_lower
			upper := new_upper
		ensure
			map_domain_constraint: map.domain >= (old map.domain & i)
			map_i_th_effect: map [i] = v
			map_old_effect: (map | old map.domain).removed (i) |=| old map.removed (i)
			map_new_effect: (map | (map.domain - old map.domain)).removed (i).is_constant (({G}).default)
			lower_effect: lower = i.min (old lower)
		end

	put (v: like item; i: INTEGER_32)
			-- Replace `i'-th entry, if in index interval, by `v'.
		note
			modify: map
		require
			valid_key: valid_index (i)
		do
			area.put (v, i - lower)
		ensure
			map_effect: map |=| old map.updated (i, v)
		end

	subcopy (other: FLAT_ARRAY [like item]; start_pos, end_pos, index_pos: INTEGER_32)
			-- Copy items of `other' within bounds `start_pos' and `end_pos'
			-- to current array starting at index `index_pos'.
		note
			modify: map
		require
			other_not_void: other /= Void
			valid_start_pos: start_pos >= other.lower
			valid_end_pos: end_pos <= other.upper
			valid_bounds: start_pos <= end_pos + 1
			valid_index_pos: index_pos >= lower
			enough_space: (upper - index_pos) >= (end_pos - start_pos)
		do
			area.copy_data (other.area, start_pos - other.lower, index_pos - lower, end_pos - start_pos + 1)
		ensure
			map_domain_unchanged: map.domain |=| old map.domain
			map_effect_old: map | (map.domain - {MML_INTERVAL} [[index_pos, index_pos + end_pos - start_pos]]) |=|
				old (map | (map.domain - {MML_INTERVAL} [[index_pos, index_pos + end_pos - start_pos]]))
			map_effect_new: ({MML_INTERVAL} [[0, end_pos - start_pos]]).for_all (agent (i: INTEGER; o: FLAT_ARRAY [G]; ip, sp: INTEGER): BOOLEAN
				do
					Result := map [ip + i] = o.map [sp + i]
				end (?, other, index_pos, start_pos))
		end

feature {FLAT_ARRAY} -- Element change

	set_area (other: like area)
			-- Make `other' the new area
			-- (from TO_SPECIAL)
		do
			area := other
		ensure -- from TO_SPECIAL
			area_set: area = other
		end

feature -- Removal

	clear_all
			-- Reset all items to default values.
		note
			modify: map
		require
			has_default: ({G}).has_default
		do
			area.fill_with (({G}).default, 0, area.count - 1)
		ensure
			map_domain_unchanged: map.domain |=| old map.domain
			map_effect: map.is_constant (({G}).default)
		end

	discard_items
			-- Reset all items to default values with reallocation.
		note
			modify: map
		require
			has_default: ({G}).has_default
		do
			create area.make_filled (({G}).default, capacity)
		ensure
			map_domain_unchanged: map.domain |=| old map.domain
			map_effect: map.is_constant (({G}).default)
		end

	keep_head (n: INTEGER_32)
			-- Remove all items except for the first `n';
			-- do nothing if `n' >= count.
		note
			modify: map
		require
			non_negative_argument: n >= 0
		do
			if n < count then
				upper := lower + n - 1
				area := area.aliased_resized_area (n)
			end
		ensure
			map_effect: map |=| old (map | {MML_INTERVAL} [[lower, lower + n - 1]])
		end

	keep_tail (n: INTEGER_32)
			-- Remove all items except for the last `n';
			-- do nothing if `n' >= count.
		note
			modify: map, lower
		require
			non_negative_argument: n >= 0
		local
			nb: INTEGER_32
		do
			nb := count
			if n < nb then
				area.overlapping_move (nb - n, 0, n)
				lower := upper - n + 1
				area := area.aliased_resized_area (n)
			end
		ensure
			map_effect: map |=| old (map | {MML_INTERVAL} [[upper - n + 1, upper]])
			lower_effect: lower = old upper - n + 1
		end

	remove_head (n: INTEGER_32)
			-- Remove first `n' items;
			-- if `n' > count, remove all.
		note
			modify: map, lower
		require
			n_non_negative: n >= 0
		do
			if n > count then
				upper := lower - 1
				area := area.aliased_resized_area (0)
			else
				keep_tail (count - n)
			end
		ensure
			map_effect: map |=| old (map | {MML_INTERVAL} [[lower + n, upper]])
			lower_effect: lower = old lower + n
		end

	remove_tail (n: INTEGER_32)
			-- Remove last `n' items;
			-- if `n' > count, remove all.
		note
			modify: map
		require
			n_non_negative: n >= 0
		do
			if n > count then
				upper := lower - 1
				area := area.aliased_resized_area (0)
			else
				keep_head (count - n)
			end
		ensure
			map_effect: map |=| old (map | {MML_INTERVAL} [[lower, upper - n]])
		end

feature -- Resizing

	automatic_grow
			-- Change the capacity to accommodate at least
			-- Growth_percentage more items.
			-- (from RESIZABLE)
		note
			modify: map
		require
			resizable: resizable
		do
			grow (capacity + additional_space)
		ensure
			map_domain_constraint: map.domain >= old map.domain
			map_count_constraint: map.count >= old (map.count * (growth_percentage / 100)).floor
			map_old_effect: (map | old map.domain) |=| old map
			map_new_effect: (map | (map.domain - old map.domain)).is_constant (({G}).default)
		end

	conservative_resize (min_index, max_index: INTEGER_32)
			-- Rearrange array so that it can accommodate
			-- indices down to `min_index' and up to `max_index'.
			-- Do not lose any previously entered item.
		note
			modify: map, lower
		require
			good_indices: min_index <= max_index
			has_default: ({G}).has_default
		do
			conservative_resize_with_default (({G}).default, min_index, max_index)
		ensure
			map_domain_constraint: map.domain |=| ({MML_INTERVAL} [[min_index, max_index]] + old map.domain)
			map_old_effect: (map | old map.domain) |=| old map
			map_new_effect: (map | (map.domain - old map.domain)).is_constant (({G}).default)
			lower_effect: lower = min_index.min (old lower)
		end

	conservative_resize_with_default (a_default_value: G; min_index, max_index: INTEGER_32)
			-- Rearrange array so that it can accommodate
			-- indices down to `min_index' and up to `max_index'.
			-- Do not lose any previously entered item.
		note
			modify: map, lower
		require
			good_indices: min_index <= max_index
		local
			new_size: INTEGER_32
			new_lower, new_upper: INTEGER_32
			offset: INTEGER_32
		do
			if empty_area then
				set_area (area.aliased_resized_area_with_default (a_default_value, max_index - min_index + 1))
				lower := min_index
				upper := max_index
			else
				new_lower := min_index.min (lower)
				new_upper := max_index.max (upper)
				new_size := new_upper - new_lower + 1
				if new_size > area.count then
					set_area (area.aliased_resized_area_with_default (a_default_value, new_size))
				end
				if new_lower < lower then
					offset := lower - new_lower
					area.move_data (0, offset, upper - lower + 1)
					area.fill_with (a_default_value, 0, offset - 1)
				end
				lower := new_lower
				upper := new_upper
			end
		ensure
			map_domain_effect: map.domain |=| ({MML_INTERVAL} [[min_index, max_index]] + old map.domain)
			map_old_effect: (map | old map.domain) |=| old map
			map_new_effect: (map | (map.domain - old map.domain)).is_constant (a_default_value)
			lower_effect: lower = min_index.min (old lower)
		end

	grow (i: INTEGER_32)
			-- Change the capacity to at least `i'.
		note
			modify: map
		require
			resizable: resizable
		do
			if i > capacity then
				conservative_resize_with_default (({G}).default, lower, upper + i - capacity)
			end
		ensure
			map_domain_constraint: map.domain >= old map.domain
			map_count_constraint: map.count >= i
			map_old_effect: (map | old map.domain) |=| old map
			map_new_effect: (map | (map.domain - old map.domain)).is_constant (({G}).default)
		end

	rebase (a_lower: like lower)
			-- Without changing the actual content of `Current' we set lower to `a_lower'
			-- and upper accordingly to `a_lower + count - 1'.
		note
			modify: map, lower
		local
			l_old_lower: like lower
		do
			l_old_lower := lower
			lower := a_lower
			upper := a_lower + (upper - l_old_lower)
		ensure
			map_domain_effect: map.domain |=| {MML_INTERVAL} [[a_lower, a_lower + old map.count - 1]]
			map_effect: ({MML_INTERVAL} [[0, old map.count - 1]]).for_all (agent (i: INTEGER; m: MML_MAP [INTEGER, G]; l1, l2: INTEGER): BOOLEAN
					do
						Result := m [l1 + i] = map [l2 + i]
					end (?, old map, old lower, a_lower))
			lower_effect: lower = a_lower
		end

	trim
			-- Decrease capacity to the minimum value.
			-- Apply to reduce allocated storage.
		local
			n: like count
		do
			n := count
			if n < capacity then
				area := area.aliased_resized_area (n)
			end
		end

feature -- Conversion

	linear_representation: FLAT_DYNAMIC_LIST [G]
			-- Representation as a linear structure
		local
			temp: FLAT_ARRAYED_LIST [G]
			i: INTEGER_32
		do
			create temp.make (capacity)
			from
				i := lower
			until
				i > upper
			loop
				temp.extend (item (i))
				i := i + 1
			end
			Result := temp
		ensure
			sequence_domain_definition: Result.sequence.count = map.count
			sequence_definition: Result.sequence.domain.for_all (agent (j: INTEGER; l: FLAT_DYNAMIC_LIST [G]): BOOLEAN
				do
					Result := l.sequence [j] = map [lower + j - 1]
				end (?, Result))
			object_comparison_definition: not Result.object_comparison
		end

	to_c: ANY
			-- Address of actual sequence of values,
			-- for passing to external (non-Eiffel) routines.
		require
			not_is_dotnet: not {PLATFORM}.is_dotnet
		do
			Result := area
		end

	to_special: SPECIAL [G]
			-- 'area'.
		do
			Result := area
		ensure
			count_effect: Result.count = map.count
			elements_effect: map.domain.for_all (agent (i: INTEGER; s: SPECIAL [G]): BOOLEAN
				do
					Result := s [i - lower] = map [i]
				end (?, Result))
		end

feature -- Duplication

	copy (other: like Current)
			-- Reinitialize by copying all the items of `other'.
			-- (This is also used by clone.)
		note
			modify: map, lower, object_comparison
		do
			if other /= Current then
				standard_copy (other)
				set_area (other.area.twin)
			end
		ensure then
			map_effect: map |=| other.map
			lower_effect: lower = other.lower
			object_comparison_effect: object_comparison = other.object_comparison
		end

	subarray (start_pos, end_pos: INTEGER_32): FLAT_ARRAY [G]
			-- Array made of items of current array within
			-- bounds `start_pos' and `end_pos'.
		require
			valid_start_pos: valid_index (start_pos)
			valid_end_pos: end_pos <= upper
			valid_bounds: (start_pos <= end_pos) or (start_pos = end_pos + 1)
		do
			if start_pos <= end_pos then
				create Result.make_filled (item (start_pos), start_pos, end_pos)
				Result.subcopy (Current, start_pos, end_pos, start_pos)
			else
				create Result.make_empty
				Result.rebase (start_pos)
			end
		ensure
			map_definition: Result.map |=| (map | {MML_INTERVAL} [[start_pos, end_pos]])
			lower_definition: Result.lower = start_pos
			object_comparison_definition: not Result.object_comparison
		end

feature {FLAT_ARRAY, FLAT_ARRAYED_LIST, FLAT_ARRAYED_SET} -- Implementation

	area: SPECIAL [G]
			-- Special data zone
			-- (from TO_SPECIAL)

	additional_space: INTEGER_32
			-- Proposed number of additional items
			-- (from RESIZABLE)
		do
			Result := (capacity // 2).max (Minimal_increase)
		ensure -- from RESIZABLE
			at_least_one: Result >= 1
		end

feature {NONE} -- Implementation

	empty_area: BOOLEAN
			-- Is area empty?
		do
			Result := area = Void or else area.capacity = 0
		end

feature -- Iteration

	do_all (action: PROCEDURE [ANY, TUPLE [G]])
			-- Apply `action' to every item, from first to last.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
		require
			action_not_void: action /= Void
		do
			area.do_all_in_bounds (action, 0, count - 1)
		end

	do_all_with_index (action: PROCEDURE [ANY, TUPLE [G, INTEGER_32]])
			-- Apply `action' to every item, from first to last.
			-- `action' receives item and its index.
			-- Semantics not guaranteed if `action' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
		local
			i, j, nb: INTEGER_32
			l_area: like area
		do
			from
				i := 0
				j := lower
				nb := count - 1
				l_area := area
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
		require
			action_not_void: action /= Void
			test_not_void: test /= Void
		do
			area.do_if_in_bounds (action, test, 0, count - 1)
		end

	do_if_with_index (action: PROCEDURE [ANY, TUPLE [G, INTEGER_32]]; test: FUNCTION [ANY, TUPLE [G, INTEGER_32], BOOLEAN])
			-- Apply `action' to every item that satisfies `test', from first to last.
			-- `action' and `test' receive the item and its index.
			-- Semantics not guaranteed if `action' or `test' changes the structure;
			-- in such a case, apply iterator to clone of structure instead.
		local
			i, j, nb: INTEGER_32
			l_area: like area
		do
			from
				i := 0
				j := lower
				nb := count - 1
				l_area := area
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
		require
			test_not_void: test /= Void
		do
			Result := area.for_all_in_bounds (test, 0, count - 1)
		ensure
			definition: Result = map.range.for_all (test)
		end

	there_exists (test: PREDICATE [ANY, TUPLE [G]]): BOOLEAN
			-- Is `test' true for at least one item?
		require
			test_not_void: test /= Void
		do
			Result := area.there_exists_in_bounds (test, 0, count - 1)
		ensure
			definition: Result = map.range.exists (test)
		end

feature -- Speacification

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

	occurrences_that (m: MML_MAP [INTEGER, G]; pred: PREDICATE [ANY, TUPLE [G]]): INTEGER
			-- Number of elements of `m' that satisfy `pred'.
		note
			status: specification
		do
			Result := m.inverse.restricted (m.range | pred).count
		ensure
			definition: Result = m.inverse.restricted (m.range | pred).count
		end

invariant
	upper_definition: upper = lower + map.count - 1

	count = area.count

	area_exists: area /= Void

end -- class FLAT_ARRAY

