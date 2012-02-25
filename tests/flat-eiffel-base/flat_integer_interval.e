note
	model: set, object_comparison

class
	FLAT_INTEGER_INTERVAL

inherit
	FLAT_READABLE_INDEXABLE [INTEGER]
		redefine
			copy,
			is_equal,
			lower, -- specification
			upper
		end

	V_ORDER [INTEGER]
		redefine
			copy,
			is_equal
		end
create
	make

feature {NONE} -- Initialization

	make (min_index, max_index: INTEGER_32)
			-- Set up interval to have bounds `min_index' and
			-- `max_index' (empty if `min_index' > `max_index')
		do
			lower_defined := True
			upper_defined := True
			if min_index <= max_index then
				lower_internal := min_index
				upper_internal := max_index
			else
				lower_internal := 1
				upper_internal := 0
			end
		ensure
			set_effect: set |=| {MML_INTERVAL} [[min_index, max_index]]
			object_comparison_effect: not object_comparison
		end

feature -- Initialization

	adapt (other: FLAT_INTEGER_INTERVAL)
			-- Reset to be the same interval as `other'.
		note
			modify: set
		require
			other_not_void: other /= Void
		do
			lower_internal := other.lower_internal
			upper_internal := other.upper_internal
			lower_defined := other.lower_defined
			upper_defined := other.upper_defined
		ensure
			set_effect: set |=| other.set
		end

feature -- Access

	at alias "@" (i: INTEGER_32): INTEGER_32
			-- Entry at index `i', if in index interval
			-- Was declared in FLAT_INTEGER_INTERVAL as synonym of item.
		require -- from TABLE
			valid_key: valid_index (i)
		do
			Result := i
		end

	has (v: INTEGER_32): BOOLEAN
			-- Does `v' appear in interval?
			-- Was declared in FLAT_INTEGER_INTERVAL as synonym of valid_index.
		do
			Result := (upper_defined implies v <= upper) and (lower_defined implies v >= lower)
		ensure
			definition: Result = set [v]
		end

	item alias "[]" (i: INTEGER_32): INTEGER_32
			-- Entry at index `i', if in index interval
			-- Was declared in FLAT_INTEGER_INTERVAL as synonym of at.
		do
			Result := i
		end

	lower: INTEGER_32
			-- Smallest value in interval
		do
			Result := lower_internal
		end

	lower_defined: BOOLEAN
			-- Is there a lower bound?

	upper: INTEGER_32
			-- Largest value in interval
		do
			Result := upper_internal
		end

	upper_defined: BOOLEAN
			-- Is there an upper bound?

	valid_index (v: INTEGER_32): BOOLEAN
			-- Does `v' appear in interval?
			-- Was declared in FLAT_INTEGER_INTERVAL as synonym of has.
		do
			Result := (upper_defined implies v <= upper) and (lower_defined implies v >= lower)
		end

feature -- Measurement

	capacity: INTEGER_32
			-- Maximum number of items in interval
			-- (here the same thing as count)
		do
			check
				terminal: upper_defined and lower_defined
			end
			Result := count
		ensure
			definition: Result = set.count
		end

	count: INTEGER_32
			-- Number of items in interval
		do
			check
				finite: upper_defined and lower_defined
			end
			if upper_defined and lower_defined then
				Result := upper - lower + 1
			end
		ensure
			definition: Result = set.count
		end

	Growth_percentage: INTEGER_32 = 50
			-- Percentage by which structure will grow automatically
			-- (from RESIZABLE)

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
			-- (here: the interval itself)
		do
			Result := Current
		end

	Minimal_increase: INTEGER_32 = 5
			-- Minimal number of additional items
			-- (from RESIZABLE)

	occurrences (v: INTEGER_32): INTEGER_32
			-- Number of times `v' appears in structure
		do
			if has (v) then
				Result := 1
			end
		ensure
			definition_has: set [v] implies Result = 1
			definition_not_has: not set [v] implies Result = 0
		end

feature -- Comparison

	is_equal (other: like Current): BOOLEAN
			-- Is array made of the same items as `other'?
		do
			Result := (lower_defined implies (other.lower_defined and lower = other.lower)) and (upper_defined implies (other.upper_defined and upper = other.upper))
		ensure then
			definition: Result = (set |=| other.set)
		end

feature -- Status report

	all_cleared: BOOLEAN
			-- Are all items set to default values?
		do
			Result := ((lower = 0) and (upper = 0))
		ensure
			definition: Result = (set |=| create {MML_SET [INTEGER]}.singleton (0))
		end

	extendible: BOOLEAN
			-- May new items be added?
			-- Answer: yes
		do
			Result := True
		ensure
			definition: Result
		end

	full: BOOLEAN
			-- Is structure full?
			-- (from BOUNDED)
		do
			Result := (count = capacity)
		ensure
			definition: Result
		end

	is_empty: BOOLEAN
			-- Is structure empty?
			-- (from FINITE)
		do
			Result := (count = 0)
		ensure
			definition: Result = set.is_empty
		end

	is_inserted (v: INTEGER_32): BOOLEAN
			-- Has `v' been inserted by the most recent insertion?
			-- (By default, the value returned is equivalent to calling
			-- `has (v)'. However, descendants might be able to provide more
			-- efficient implementations.)
			-- (from COLLECTION)
		do
			Result := has (v)
		ensure
			definition: Result = set [v]
		end

	object_comparison: BOOLEAN
			-- Must search operations use equal rather than `='
			-- for comparing references? (Default: no, use `='.)
			-- (from CONTAINER)

	prunable: BOOLEAN
			-- May individual items be removed?
			-- Answer: no
		do
			Result := False
		ensure
			definition: not Result
		end

	resizable: BOOLEAN
			-- May capacity be changed? (Answer: yes.)
			-- (from RESIZABLE)
		do
			Result := True
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

feature -- Element change

	extend (v: INTEGER_32)
			-- Make sure that interval goes all the way
			-- to `v' (up or down).
			-- Was declared in FLAT_INTEGER_INTERVAL as synonym of put.
		note
			modify: set
		require
			extendible: extendible
		do
			if v < lower then
				lower_internal := v
			elseif v > upper then
				upper_internal := v
			end
		ensure
			set_effect: set |=| {MML_INTERVAL} [[v.min (old lower), v.max (old upper)]]
		end

	fill (other: FLAT_DYNAMIC_LIST [INTEGER_32])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current structure
			-- need not be the same.
			-- (from COLLECTION)
		note
			modify: set
		require -- from COLLECTION
			other_not_void: other /= Void
			extendible: extendible
		local
			lin_rep: FLAT_DYNAMIC_LIST [INTEGER_32]
		do
			lin_rep := other.linear_representation
			from
				lin_rep.start
			until
				not extendible or else lin_rep.off
			loop
				extend (lin_rep.item)
				lin_rep.forth
			end
		ensure
			set_effect: set |=| {MML_INTERVAL} [[other.sequence.range.extremum (agent less_equal).min (old lower),
				other.sequence.range.extremum (agent greater_equal).max (old upper)]]
		end

	put (v: INTEGER_32)
			-- Make sure that interval goes all the way
			-- to `v' (up or down).
			-- Was declared in FLAT_INTEGER_INTERVAL as synonym of extend.
		note
			modify: set
		require
			extendible: extendible
		do
			if v < lower then
				lower_internal := v
			elseif v > upper then
				upper_internal := v
			end
		ensure
			set_effect: set |=| {MML_INTERVAL} [[v.min (old lower), v.max (old upper)]]
		end

feature -- Removal

	changeable_comparison_criterion: BOOLEAN
			-- May object_comparison be changed?
			-- (Answer: only if set empty; otherwise insertions might
			-- introduce duplicates, destroying the set property.)
			-- (from SET)
		do
			Result := is_empty
		ensure
			definition: Result = set.is_empty
		end

	wipe_out
			-- Remove all items.
		note
			modify: set
--		require
--			prunable: prunable
		do
			lower_defined := True
			upper_defined := True
			lower_internal := 1
			upper_internal := 0
		ensure
			set_effect: set.is_empty
		end

feature -- Resizing

	automatic_grow
			-- Change the capacity to accommodate at least
			-- Growth_percentage more items.
			-- (from RESIZABLE)
		note
			modify: set
		require -- from RESIZABLE
			resizable: resizable
		do
			grow (capacity + additional_space)
		ensure
			set_constraint: set >= old set
		end

	grow (i: INTEGER_32)
			-- Ensure that capacity is at least `i'.
		note
			modify: set
		require
			resizable: resizable
		do
			if capacity < i then
				resize (lower, lower + i - 1)
			end
		ensure
			set_constraint: set >= old set
			set_count_constraint: set.count >= i
		end

	resize (min_index, max_index: INTEGER_32)
			-- Rearrange interval to go from at most
			-- `min_index' to at least `max_index',
			-- encompassing previous bounds.
		note
			modify: set
		do
			lower_internal := min_index.min (lower)
			upper_internal := max_index.max (upper)
		ensure
			set_effect: set |=| {MML_INTERVAL} [[min_index.min (old lower), max_index.max (old upper)]]
		end

	resize_exactly (min_index, max_index: INTEGER_32)
			-- Rearrange interval to go from
			-- `min_index' to `max_index'.
		note
			modify: set
		do
			lower_internal := min_index
			upper_internal := max_index
		ensure
			-- ToDo: violates the invariant that set.is_empty implies lower = 1 and upper = 0
			-- and even a less strict invariant lower <= upper + 1, causing count < 0
			set_effect: set |=| {MML_INTERVAL} [[min_index, max_index]]
		end

	trim
			-- Decrease capacity to the minimum value.
			-- Apply to reduce allocated storage.
		do
			check
				minimal_capacity: capacity = count
			end
		end

feature -- Conversion

	as_array: FLAT_ARRAY [INTEGER_32]
			-- Plain array containing interval's items
		local
			i: INTEGER_32
		do
			create Result.make_empty
			Result.rebase (lower)
			from
				i := lower
			until
				i > upper
			loop
				Result.force (i, i)
				i := i + 1
			end
		ensure
			map_definition: Result.map |=| map
			lower_definition: Result.lower = lower
			object_comparison_definition: not Result.object_comparison
		end

	linear_representation: FLAT_DYNAMIC_LIST [INTEGER_32]
			-- Representation as a linear structure
		do
			check
				terminal: upper_defined and lower_defined
			end
			Result := as_array.linear_representation
		ensure
			sequence_domain_definition: Result.sequence.count = set.count
			sequence_definition: Result.sequence.domain.for_all (agent (i: INTEGER; l: FLAT_DYNAMIC_LIST [INTEGER]): BOOLEAN
				do
					Result := l.sequence [i] = lower + i - 1
				end (?, Result))
			object_comparison_definition: not Result.object_comparison
		end

feature -- Duplication

	copy (other: like Current)
			-- Reset to be the same interval as `other'.
		note
			modify: set, object_comparison
		do
			if other /= Current then
				standard_copy (other)
			end
		ensure then
			set_effect: set |=| other.set
			object_comparison_effect: object_comparison = other.object_comparison
		end

	subinterval (start_pos, end_pos: INTEGER_32): like Current
			-- Interval made of items of current array within
			-- bounds `start_pos' and `end_pos'.
		do
			create Result.make (start_pos, end_pos)
		ensure
			set_definition: Result.set |=| {MML_INTERVAL} [[start_pos, end_pos]]
			object_comparison_definition: not Result.object_comparison
		end

feature {FLAT_INTEGER_INTERVAL} -- Implementation

	lower_internal: INTEGER_32
			-- See `lower`.

	upper_internal: INTEGER_32
			-- See `upper`.

	additional_space: INTEGER_32
			-- Proposed number of additional items
			-- (from RESIZABLE)
		do
			Result := (capacity // 2).max (Minimal_increase)
		ensure -- from RESIZABLE
			at_least_one: Result >= 1
		end

feature -- Iteration

	do_all (action: PROCEDURE [ANY, TUPLE [INTEGER_32]])
			-- Apply `action' to every item of current interval.
		require
			action_exists: action /= Void
			finite: upper_defined and lower_defined
		local
			i, nb: INTEGER_32
		do
			from
				i := lower
				nb := upper
			until
				i > nb
			loop
				action.call ([i])
				i := i + 1
			end
		end

	exists (condition: PREDICATE [ANY, TUPLE [INTEGER_32]]): BOOLEAN
			-- Does at least one of  interval's values
			-- satisfy `condition'?
		require
			finite: upper_defined and lower_defined
			condition_not_void: condition /= Void
		local
			i: INTEGER_32
		do
			from
				i := lower
			until
				i > upper or else condition.item ([i])
			loop
				i := i + 1
			end
			Result := (i <= upper)
		ensure
			definition: Result = set.exists (condition)
		end

	exists1 (condition: PREDICATE [ANY, TUPLE [INTEGER_32]]): BOOLEAN
			-- Does exactly one of  interval's values
			-- satisfy `condition'?
		require
			finite: upper_defined and lower_defined
			condition_not_void: condition /= Void
		do
			Result := (hold_count (condition) = 1)
		ensure
			definition: Result = ((set | condition).count = 1)
		end

	for_all (condition: PREDICATE [ANY, TUPLE [INTEGER_32]]): BOOLEAN
			-- Do all interval's values satisfy `condition'?
		require
			finite: upper_defined and lower_defined
			condition_not_void: condition /= Void
		local
			i: INTEGER_32
		do
			from
				Result := True
				i := lower
			until
				(i > upper) or else (not condition.item ([i]))
			loop
				i := i + 1
			end
			Result := (i > upper)
		ensure
			definition: Result = set.for_all (condition)
		end

	hold_count (condition: PREDICATE [ANY, TUPLE [INTEGER_32]]): INTEGER_32
			-- Number of  interval's values that
			-- satisfy `condition'
		require
			finite: upper_defined and lower_defined
			condition_not_void: condition /= Void
		local
			i: INTEGER_32
		do
			from
				i := lower
			until
				i > upper
			loop
				if condition.item ([i]) then
					Result := Result + 1
				end
				i := i + 1
			end
		ensure
			definition: Result = (set | condition).count
		end

feature -- Specification

	set: MML_INTERVAL
			-- Set of elements.
		note
			status: specification
		do
			create Result.from_range (lower, upper)
		end

invariant

	map_domain_definition: map.domain |=| set
	map_definition: map.domain.for_all (agent (i: INTEGER): BOOLEAN
		do
			Result := map [i] = i
		end)
	lower_definition_empty: set.is_empty implies lower = 1
	lower_defined_definition: lower_defined
	upper_defined_definition: upper_defined

end -- class FLAT_INTEGER_INTERVAL

