note
	model: sequence, capacity, object_comparison

class
	FLAT_BOUNDED_QUEUE [G]

inherit
	V_EQUALITY [G]
		redefine
			copy,
			is_equal
		end

create
	make

feature -- Initialization

	make (n: INTEGER_32)
			-- Create queue for at most `n' items.
			-- (from ARRAYED_QUEUE)
		require
			non_negative_argument: n >= 0
		do
			create area.make_empty (n)
			out_index := 1
			count := 0
		ensure
			sequence_effect: sequence.is_empty
			capacity_effect: capacity = n
			object_comparison_effect: not object_comparison
		end

feature -- Access

	has (v: like item): BOOLEAN
			-- Does queue include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from ARRAYED_QUEUE)
		require -- from  CONTAINER
			True
		local
			i, j, nb: INTEGER_32
		do
			i := out_index - Lower
			j := count
			nb := area.capacity
			if object_comparison then
				from
				until
					j = 0 or v ~ area.item (i)
				loop
					i := i + 1
					if i = nb then
						i := 0
					end
					j := j - 1
				end
			else
				from
				until
					j = 0 or v = area.item (i)
				loop
					i := i + 1
					if i = nb then
						i := 0
					end
					j := j - 1
				end
			end
			Result := j > 0
		ensure
			definition: Result = sequence.range.exists (agent element_equal (v, ?))
		end

	item: G
			-- Oldest item.
			-- (from ARRAYED_QUEUE)
		require
			readable: readable
		do
			Result := area.item (out_index - Lower)
		ensure
			definition: Result = sequence.first
		end

feature -- Measurement

	capacity: INTEGER_32
			-- Number of items that may be stored
			-- (from ARRAYED_QUEUE)
		require -- from  BOUNDED
			True
		do
			Result := area.capacity
		ensure -- from BOUNDED
			capacity_non_negative: Result >= 0
		end

	count: INTEGER_32
			-- Number of items
			-- (from ARRAYED_QUEUE)

	Growth_percentage: INTEGER_32 = 50
			-- Percentage by which structure will grow automatically
			-- (from RESIZABLE)

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
			-- (from ARRAYED_QUEUE)
		do
			create Result.make (1, count)
		ensure
			set_definition: Result.set |=| sequence.domain
			object_comparison_definition: not Result.object_comparison
		end

	Minimal_increase: INTEGER_32 = 5
			-- Minimal number of additional items
			-- (from RESIZABLE)

	occurrences (v: G): INTEGER_32
			-- Number of times `v' appears in structure
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from ARRAYED_QUEUE)
		local
			i, j, nb: INTEGER_32
		do
			i := out_index - Lower
			j := count
			nb := area.capacity
			if object_comparison then
				from
				until
					j = 0
				loop
					if area.item (i) ~ v then
						Result := Result + 1
					end
					i := i + 1
					if i = nb then
						i := 0
					end
					j := j - 1
				end
			else
				from
				until
					j = 0
				loop
					if area.item (i) = v then
						Result := Result + 1
					end
					i := i + 1
					if i = nb then
						i := 0
					end
					j := j - 1
				end
			end
		ensure
			definition: Result = occurrences_that (sequence, agent element_equal (v, ?))
		end

feature -- Comparison

	is_equal (other: FLAT_BOUNDED_QUEUE [G]): BOOLEAN
			-- Is `other' attached to an object considered
			-- equal to current object?
			-- (from ARRAYED_QUEUE)
		local
			i, j: INTEGER_32
			nb, other_nb: INTEGER_32
			c: INTEGER_32
		do
			c := count
			if c = other.count and object_comparison = other.object_comparison then
				i := out_index - Lower
				j := other.out_index - Lower
				nb := area.capacity
				other_nb := other.area.capacity
				Result := True
				if object_comparison then
					from
					until
						c = 0 or not Result
					loop
						Result := area.item (i) ~ other.area.item (j)
						j := j + 1
						if j > other_nb then
							j := 0
						end
						i := i + 1
						if i = nb then
							i := 0
						end
						c := c - 1
					end
				else
					from
					until
						c = 0 or not Result
					loop
						Result := area.item (i) = other.area.item (j)
						j := j + 1
						if j > other_nb then
							j := 0
						end
						i := i + 1
						if i = nb then
							i := 0
						end
						c := c - 1
					end
				end
			end
		ensure then
			definition: Result = (object_comparison = other.object_comparison and
				sequence.count = other.sequence.count and
				sequence.domain.for_all (agent (k: INTEGER; o: FLAT_BOUNDED_QUEUE [G]): BOOLEAN
					do
						Result := element_equal (o.sequence [k], sequence [k])
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
			-- May items be added? (Answer: yes.)
		do
			Result := not full
		ensure
			definition: Result = (sequence.count < capacity)
		end

	full: BOOLEAN
			-- Is structure full?
			-- (from BOUNDED)
		do
			Result := (count = capacity)
		ensure
			definition: Result = (sequence.count = capacity)
		end

	is_empty: BOOLEAN
			-- Is the structure empty?
			-- Was declared in ARRAYED_QUEUE as synonym of off.
			-- (from ARRAYED_QUEUE)
		do
			Result := count = 0
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

	off: BOOLEAN
			-- Is the structure empty?
			-- Was declared in ARRAYED_QUEUE as synonym of is_empty.
			-- (from ARRAYED_QUEUE)
		do
			Result := count = 0
		ensure
			definition: Result = sequence.is_empty
		end

	prunable: BOOLEAN
			-- May items be removed? (Answer: no.)
			-- (from ARRAYED_QUEUE)
		do
			Result := False
		ensure
			definition: not Result
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

	resizable: BOOLEAN
			-- May capacity be changed? (Answer: yes.)
			-- (from RESIZABLE)
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

	append (s: FLAT_DYNAMIC_LIST [G])
			-- Append a copy of `s'.
			-- (Synonym for fill)
			-- (from DISPENSER)
		note
			modify: sequence
		require
			s_not_void: s /= Void
			extendible: extendible
		do
			fill (s)
		ensure
			sequence_effect: sequence |=| old (sequence + s.sequence.front (capacity - sequence.count))
		end

	extend (v: G)
			-- Add `v' as newest item.
			-- Was declared in ARRAYED_QUEUE as synonym of put and force.
			-- (from ARRAYED_QUEUE)
		note
			modify: sequence
		require -- from COLLECTION
			extendible: extendible
		local
			l_capacity: like capacity
			l_count: like count
		do
			l_capacity := capacity
			l_count := count
			if l_count >= l_capacity then
				grow (l_capacity + additional_space)
				l_capacity := capacity
			end
			area.force (v, in_index - Lower)
			count := l_count + 1
		ensure
			sequence_effect: sequence |=| (old sequence & v)
		end

	fill (other: FLAT_DYNAMIC_LIST [G])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current structure
			-- need not be the same.
			-- (from COLLECTION)
		note
			modify: sequence
		require -- from COLLECTION
			other_not_void: other /= Void
			extendible: extendible
		local
			lin_rep: FLAT_DYNAMIC_LIST [G]
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
			sequence_effect: sequence |=| old (sequence + other.sequence.front (capacity - sequence.count))
		end

	force (v: G)
			-- Add `v' as newest item.
			-- Was declared in ARRAYED_QUEUE as synonym of extend and put.
			-- (from ARRAYED_QUEUE)
		note
			modify: sequence
		require
			extendible: extendible
		local
			l_capacity: like capacity
			l_count: like count
		do
			l_capacity := capacity
			l_count := count
			if l_count >= l_capacity then
				grow (l_capacity + additional_space)
				l_capacity := capacity
			end
			area.force (v, in_index - Lower)
			count := l_count + 1
		ensure
			sequence_effect: sequence |=| (old sequence & v)
		end

	put (v: G)
			-- Add `v' as newest item.
			-- Was declared in ARRAYED_QUEUE as synonym of extend and force.
			-- (from ARRAYED_QUEUE)
		note
			modify: sequence
		require
			extendible: extendible
		local
			l_capacity: like capacity
			l_count: like count
		do
			l_capacity := capacity
			l_count := count
			if l_count >= l_capacity then
				grow (l_capacity + additional_space)
				l_capacity := capacity
			end
			area.force (v, in_index - Lower)
			count := l_count + 1
		ensure
			sequence_effect: sequence |=| (old sequence & v)
		end

	replace (v: like item)
			-- Replace oldest item by `v'.
			-- (from ARRAYED_QUEUE)
		note
			modify: sequence
		require
			writable: writable
		do
			area.put (v, out_index - Lower)
		ensure
			sequence_effect: sequence |=| old sequence.replaced_at (1, v)
		end

feature -- Removal

	remove
			-- Remove oldest item.
			-- (from ARRAYED_QUEUE)
		note
			modify: sequence
		require
			writable: writable
		local
			l_removed_index: like out_index
		do
			l_removed_index := out_index
			out_index := l_removed_index \\ capacity + 1
			count := count - 1
			if count = 0 then
				wipe_out
			else
				area.put (newest_item, l_removed_index - Lower)
			end
		ensure
			sequence_effect: sequence |=| old sequence.but_first
		end

	wipe_out
			-- Remove all items.
			-- (from ARRAYED_QUEUE)
		note
			modify: sequence
		do
			area.wipe_out
			out_index := 1
			count := 0
		ensure
			sequence_effect: sequence.is_empty
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
			capacity_effect: capacity >= old capacity
		end

	trim
			-- Decrease capacity to the minimum value.
			-- Apply to reduce allocated storage.
			-- (from ARRAYED_QUEUE)
		note
			modify: capacity
		local
			i: like Lower
			j: like Lower
			n: like count
			m: like capacity
		do
			n := count
			m := capacity
			if n < m then
				i := out_index - Lower
				j := in_index - Lower
				if i < j then
					area.move_data (i, 0, n)
					out_index := Lower
				elseif n > 0 then
					area.move_data (i, j, m - i)
					out_index := j + Lower
				end
				area := area.aliased_resized_area (n)
			end
		ensure
			capacity_effect: capacity = sequence.count
		end

feature -- Conversion

	linear_representation: FLAT_ARRAYED_LIST [G]
			-- Representation as a linear structure
			-- (in the original insertion order)
			-- (from ARRAYED_QUEUE)
		local
			i, j, nb: INTEGER_32
		do
			from
				i := out_index - Lower
				j := count
				nb := area.capacity
				create Result.make (j)
			until
				j = 0
			loop
				Result.extend (area.item (i))
				i := i + 1
				if i = nb then
					i := 0
				end
				j := j - 1
			end
		ensure
			sequence_definition: Result.sequence |=| sequence
			object_comparison_effect: not Result.object_comparison
		end

feature -- Duplication

	copy (other: FLAT_BOUNDED_QUEUE [G])
			-- Update current object using fields of object attached
			-- to `other', so as to yield equal objects.
			-- (from ARRAYED_QUEUE)
		note
			modify: sequence, capacity, object_comparison
		do
			if other /= Current then
				standard_copy (other)
				area := area.twin
			end
		ensure then
			sequence_effect: sequence |=| other.sequence
			capacity_effect: capacity = other.capacity
			object_comparison_effect: object_comparison = other.object_comparison
		end

feature {FLAT_BOUNDED_QUEUE} -- Implementation

	area: SPECIAL [G]
			-- Storage for queue
			-- (from ARRAYED_QUEUE)

	grow (n: INTEGER_32)
			-- Ensure that capacity is at least `i'.
			-- (from ARRAYED_QUEUE)
		require -- from RESIZABLE
			resizable: resizable
		local
			old_count, new_capacity: like capacity
			nb: INTEGER_32
		do
			new_capacity := area.capacity.max (n)
			if count = 0 or else in_index > out_index then
				area := area.aliased_resized_area (new_capacity)
			else
				old_count := area.count
				area := area.aliased_resized_area_with_default (newest_item, new_capacity)
				nb := old_count - out_index + 1
				area.move_data (out_index - Lower, new_capacity - nb, nb)
				out_index := new_capacity - nb + 1
			end
		ensure -- from RESIZABLE
			new_capacity: capacity >= n
		end

	in_index: INTEGER_32
			-- Position for next insertion
			-- (from ARRAYED_QUEUE)
		local
			c: like capacity
		do
			c := capacity
			if c > 0 then
				Result := (out_index - Lower + count) \\ c + Lower
			else
				Result := out_index
			end
		end

	out_index: INTEGER_32
			-- Position of oldest item
			-- (from ARRAYED_QUEUE)

	additional_space: INTEGER_32
			-- Proposed number of additional items
			-- (from RESIZABLE)
		do
			Result := (capacity // 2).max (Minimal_increase)
		ensure -- from RESIZABLE
			at_least_one: Result >= 1
		end

feature {NONE} -- Implementation

	Lower: INTEGER_32 = 1
			-- Lower bound for accessing list items via indexes
			-- (from ARRAYED_QUEUE)

	newest_item: G
			-- Most recently added item.
			-- (from ARRAYED_QUEUE)
		local
			l_pos: INTEGER_32
		do
			l_pos := in_index - 1
			if l_pos = 0 then
				Result := area.item (area.upper)
			else
				Result := area.item (l_pos - Lower)
			end
		end

	upper: INTEGER_32
			-- Upper bound for accessing list items via indexes
			-- (from ARRAYED_QUEUE)
		do
			Result := area.count
		end

feature -- Specification

	sequence: MML_SEQUENCE [G]
			-- Sequence of elements (head first).
		note
			status: specification
		local
			i, j: INTEGER_32
		do
			create Result
			from
				i := out_index - 1
				j := 1
			until
				j > count
			loop
				Result := Result & area.item (i)
				i := i + 1
				if i = area.capacity then
					i := 0
				end
				j := j + 1
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

	sequence_capacity_constraint: sequence.count <= capacity
	count_definition: count = sequence.count

end -- class BOUNDED_QUEUE

