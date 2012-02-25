note
	model: map, lower

deferred class
	FLAT_READABLE_INDEXABLE [G]

feature -- Access

	item alias "[]" (i: INTEGER_32): G
			-- Entry at position `i'
		require
			valid_index: valid_index (i)
		deferred
		ensure
			definition: Result = map [i]
		end

	new_cursor: FLAT_INDEXABLE_ITERATION_CURSOR [G]
			-- Fresh cursor associated with current structure
		do
			create Result.make (Current)
			Result.start
		ensure
			target_definition: Result.target = Current
			target_index_definition: Result.target_index = lower
			step_definition: Result.step = 1
			is_reversed_definition: not Result.is_reversed
		end

feature -- Measurement

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
		deferred
		ensure
			set_definition: Result.set |=| map.domain
			object_comparison_definition: not Result.object_comparison
		end

feature -- Status report

	valid_index (i: INTEGER_32): BOOLEAN
			-- Is `i' a valid index?
		deferred
		ensure
			definition: Result = map.domain [i]
		end

feature -- Specification

	map: MML_MAP [INTEGER, G]
			-- Map from indexes to values.
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
				Result := Result.updated (c.target_index, c.item)
				c.forth
			end
		end

	lower: INTEGER
			-- Lower bound of the map domain.
		note
			status: specification
		do
			Result := index_set.lower
		end

	upper: INTEGER
			-- Upper bound of the map domain.
		note
			status: specification
		do
			Result := index_set.upper
		ensure
			definition: Result = lower + map.count - 1
		end

invariant

	indexes_in_interval: map.domain |=| {MML_INTERVAL} [[lower, upper]]

end -- class FLAT_READABLE_INDEXABLE

