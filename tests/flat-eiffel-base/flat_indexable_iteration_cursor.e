note
	model: target, target_index, step, is_reversed

class
	FLAT_INDEXABLE_ITERATION_CURSOR [G]

create
	make

feature {NONE} -- Initialization

	make (s: like target)
			-- Initialize cursor using structure `s'.
		require
			s_attached: s /= Void
		do
			target := s
			if attached {VERSIONABLE} s as l_versionable then
				version := l_versionable.version
			else
				version := 0
			end
			step := 1
			is_reversed := False
		ensure
			target_effect: target = s
			target_index_effect: target_index = 0
			step_effect: step = 1
			is_reversed_effect: not is_reversed
		end

feature -- Access

	target: FLAT_READABLE_INDEXABLE [G]
			-- Associated structure used for iteration.	

	cursor_index: INTEGER_32
			-- Index position of cursor in the iteration.
		do
			if is_reversed then
				Result := index_set.upper - target_index + 1
			else
				Result := target_index - index_set.lower + 1
			end
		ensure
			definition_not_reversed: not is_reversed implies Result = target_index - target.lower + 1
			definition_reversed: is_reversed implies Result = target.upper - target_index + 1
		end

	decremented alias "-" (n: like step): like Current
			-- Copy of Current with step decreased by `n'.
		require
			n_valid: step > n
		do
			Result := twin
			Result.set_step (step - n)
		ensure
			target_definition: Result.target = target
			target_index_definition: Result.target_index = target_index
			step_definition: Result.step = step - n
			is_reversed_definition: Result.is_reversed = is_reversed
		end

	incremented alias "+" (n: like step): like Current
			-- Copy of Current with step increased by `n'.
		require
			n_valid: step + n > 0
		do
			Result := twin
			Result.set_step (step + n)
		ensure
			target_definition: Result.target = target
			target_index_definition: Result.target_index = target_index
			step_definition: Result.step = step + n
			is_reversed_definition: Result.is_reversed = is_reversed
		end

	item: G
			-- Item at current cursor position.
		require -- from ITERATION_CURSOR
			valid_position: target.map.domain [target_index]
		do
			Result := target.item (target_index)
		ensure
			definition: Result = target.map [target_index]
		end

	new_cursor: FLAT_INDEXABLE_ITERATION_CURSOR [G]
			-- Restarted copy of Current.
		do
			Result := twin
			Result.start
		ensure -- from ITERABLE
			target_definition: Result.target = target
			cursor_index_definition: Result.cursor_index = 1
			step_definition: Result.step = step
			is_reversed_definition: Result.is_reversed = is_reversed
		end

	reversed alias "-": like Current
			-- Reversed copy of Current.
		do
			Result := twin
			Result.reverse
		ensure
			target_definition: Result.target = target
			target_index_definition: Result.target_index = target_index
			step_definition: Result.step = step
			is_reversed_definition: Result.is_reversed = not is_reversed
		end

	step: INTEGER_32
			-- Distance between successive iteration elements.

	target_index: INTEGER_32
			-- Index position of target for current iteration.

	with_step (n: like step): like Current
			-- Copy of Current with step set to `n'.
		require
			n_positive: n > 0
		do
			Result := twin
			Result.set_step (n)
		ensure
			target_definition: Result.target = target
			target_index_definition: Result.target_index = target_index
			step_definition: Result.step = n
			is_reversed_definition: Result.is_reversed = is_reversed
		end

feature -- Measurement

	version: NATURAL_32
			-- Current version.
		note
			option: transient
		attribute
		ensure
			definition: Result = 0
		end

feature -- Status report

	after: BOOLEAN
			-- Are there no more items to iterate over?
		do
			Result := not is_valid or not target.valid_index (target_index)
		ensure
			definition: Result = not target.map.domain [target_index]
		end

	is_reversed: BOOLEAN
			-- Are we traversing target backwards?

	is_valid: BOOLEAN
			-- Is the cursor still compatible with the associated underlying object?
		do
			Result := attached {VERSIONABLE} target as l_versionable implies l_versionable.version = version
		ensure
			definition: Result
		end

feature -- Status setting

	reverse
			-- Flip traversal order.
		note
			modify: is_reversed
		do
			is_reversed := not is_reversed
		ensure
			is_reversed_effect: is_reversed = not old is_reversed
		end

	set_step (v: like step)
			-- Set increment step to `v'.
		note
			modify: step
		require
			v_positive: v > 0
		do
			step := v
		ensure
			step_effect: step = v
		end

feature -- Cursor movement

	forth
			-- Move to next position.
		note
			modify: target_index
		require -- from ITERATION_CURSOR
			valid_position: not after
		do
			if is_reversed then
				target_index := target_index - step
			else
				target_index := target_index + step
			end
		ensure then
			cursor_index_effect: cursor_index = old cursor_index + 1
		end

	start
		note
			modify: target_index
		do
			if is_reversed then
				target_index := index_set.upper
			else
				target_index := index_set.lower
			end
		ensure then
			cursor_index_effect: cursor_index = 1
		end


feature {FLAT_READABLE_INDEXABLE} -- Implementation

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes for target.
		do
			Result := target.index_set
		ensure
			set_definition: Result.set |=| target.map.domain
			object_comparison_definition: not Result.object_comparison
		end

invariant
	target_attached: target /= Void
	step_positive: step > 0

end -- class FLAT_INDEXABLE_ITERATION_CURSOR

