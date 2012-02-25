note
	model: target, iteration_position, step, is_reversed

class
	FLAT_HASH_TABLE_ITERATION_CURSOR [G, K -> detachable HASHABLE]

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
			iteration_position_effect: iteration_position = 0
			step_effect: step = 1
			is_reversed_effect: not is_reversed
		end

feature -- Access

	item: G
			-- Item at current cursor position.
		require
			valid_position: target.iteration_map.domain [iteration_position]
		do
			Result := target.content [iteration_position]
		ensure
			definition: Result = target.map [target.iteration_map [iteration_position]]
		end

	key: K
			-- Key at current cursor position
		require
			valid_position: not after
		do
			Result := target.keys [iteration_position]
		ensure
			definition: Result = target.iteration_map [iteration_position]
		end

	target: FLAT_HASH_TABLE [G, K]
			-- Associated structure used for iteration.	

	cursor_index: INTEGER_32
			-- Index position of cursor in the iteration.
		do
			if is_reversed then
				Result := index_set.upper - iteration_position + 1
			else
				Result := iteration_position - index_set.lower + 1
			end
		ensure
			definition_not_reversed_not_empty: not is_reversed and not target.map.is_empty implies Result = iteration_position - target.lower + 1
			definition_reversed_not_empty: is_reversed and not target.map.is_empty implies Result = target.upper - iteration_position + 1
			definition_not_reversed_empty: not is_reversed and target.map.is_empty implies Result = iteration_position
			definition_reversed_empty: is_reversed and target.map.is_empty implies Result = 1 - iteration_position
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
			iteration_position_definition: Result.iteration_position = iteration_position
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
			iteration_position_definition: Result.iteration_position = iteration_position
			step_definition: Result.step = step + n
			is_reversed_definition: Result.is_reversed = is_reversed
		end

	new_cursor: FLAT_HASH_TABLE_ITERATION_CURSOR [G, K]
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
			iteration_position_definition: Result.iteration_position = iteration_position
			step_definition: Result.step = step
			is_reversed_definition: Result.is_reversed = not is_reversed
		end

	step: INTEGER_32
			-- Distance between successive iteration elements.

	iteration_position: INTEGER_32
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
			iteration_position_definition: Result.iteration_position = iteration_position
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
		local
			l_pos: like iteration_position
		do
			l_pos := iteration_position
			Result := not is_valid or l_pos < 0 or l_pos >= target.keys.count
		ensure
			definition: Result = not target.iteration_map.domain [iteration_position]
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
			modify: iteration_position
		require
			valid_position: not after
		local
			i, nb: like step
			l_pos: like iteration_position
		do
			l_pos := iteration_position
			nb := step
			if is_reversed then
				from
					i := 1
				until
					i > nb or else l_pos < 0
				loop
					l_pos := target.previous_iteration_position (l_pos)
					i := i + 1
				end
			else
				from
					i := 1
				until
					i > nb or else l_pos >= target.keys.count
				loop
					l_pos := target.next_iteration_position (l_pos)
					i := i + 1
				end
			end
			iteration_position := l_pos
		ensure then
			cursor_index_effect: cursor_index > old cursor_index
		end

	start
		note
			modify: iteration_position
		do
			if is_reversed then
				iteration_position := index_set.upper
			else
				iteration_position := index_set.lower
			end
		ensure then
			cursor_index_effect: cursor_index = 1
		end

feature {FLAT_READABLE_INDEXABLE} -- Implementation

	index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes for target.
		do
			Result := target.iteration_index_set
		ensure
			set_definition: Result.set |=| {MML_INTERVAL} [[target.lower, target.upper]]
			object_comparison_definition: not Result.object_comparison
		end

invariant
	target_attached: target /= Void
	step_positive: step > 0

end
