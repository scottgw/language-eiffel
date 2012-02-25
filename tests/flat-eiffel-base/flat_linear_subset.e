note
	model: sequence, index, object_comparison

deferred class
	FLAT_LINEAR_SUBSET [G]

inherit
	V_EQUALITY [G]

feature -- Access

	has (v: G): BOOLEAN
			-- Does structure include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from CONTAINER)
		deferred
		ensure
			definition: Result = set_has (set, v)
		end

	index: INTEGER_32
			-- Current index
		deferred
		end

	item: G
			-- Current item
			-- (from TRAVERSABLE_SUBSET)
		require
			not_off: not off
		deferred
		ensure
			definition: Result = sequence [index]
		end

feature -- Measurement

	count: INTEGER_32
			-- Number of items
			-- (from TRAVERSABLE_SUBSET)
		deferred
		ensure
			definition: Result = sequence.count
		end

feature -- Comparison

	disjoint (other: FLAT_LINEAR_SUBSET [G]): BOOLEAN
			-- Do current set and `other' have no
			-- items in common?
			-- (from TRAVERSABLE_SUBSET)
		require
			set_exists: other /= Void
			same_rule: object_comparison = other.object_comparison
		local
			s: FLAT_SUBSET_STRATEGY [G]
		do
			if not is_empty and not other.is_empty then
				s := subset_strategy (other)
				Result := s.disjoint (Current, other)
			else
				Result := True
			end
		ensure
			definition: Result = not other.set.exists (agent set_has (set, ?))
		end

	is_subset (other: FLAT_LINEAR_SUBSET [G]): BOOLEAN
			-- Is current set a subset of `other'?
			-- (from TRAVERSABLE_SUBSET)
		require
			set_exists: other /= Void
			same_rule: object_comparison = other.object_comparison
		do
			if not other.is_empty and then count <= other.count then
				from
					start
				until
					off or else not other.has (item)
				loop
					forth
				end
				if off then
					Result := True
				end
			elseif is_empty then
				Result := True
			end
		ensure
			definition: Result = set.for_all (agent set_has (other.set, ?))
		end

	is_superset (other: FLAT_LINEAR_SUBSET [G]): BOOLEAN
			-- Is current set a superset of `other'?
			-- (from SUBSET)
		require
			set_exists: other /= Void
			same_rule: object_comparison = other.object_comparison
		do
			Result := other.is_subset (Current)
		ensure
			definition: Result = other.set.for_all (agent set_has (set, ?))
		end

feature -- Status report

	after: BOOLEAN
			-- Is cursor behind last item?
			-- (from TRAVERSABLE_SUBSET)
		deferred
		ensure
			definition: Result = (index > sequence.count)
		end

	before: BOOLEAN
			-- Is cursor at left from first item?
		deferred
		ensure
			definition: Result = (index < 1)
		end

	extendible: BOOLEAN
			-- May new items be added?
			-- (from COLLECTION)
		deferred
		ensure
			definition: Result
		end

	is_empty: BOOLEAN
			-- Is container empty?
			-- (from TRAVERSABLE_SUBSET)
		deferred
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
			definition: Result = set_has (set, v)
		end

	islast: BOOLEAN
			-- Is cursor at last item?
		deferred
		ensure
			definition: Result = (not sequence.is_empty and index = sequence.count)
		end

	object_comparison: BOOLEAN
			-- Must search operations use equal rather than `='
			-- for comparing references? (Default: no, use `='.)
			-- (from CONTAINER)

	off: BOOLEAN
			-- Is cursor off the active items?
			-- (from TRAVERSABLE_SUBSET)
		deferred
		ensure
			definition: Result = not sequence.domain [index]
		end

	prunable: BOOLEAN
			-- May items be removed?
			-- (from COLLECTION)
		deferred
		ensure
			definition: Result
		end

	valid_index (n: INTEGER_32): BOOLEAN
			-- Is `n' a valid index?
		deferred
		ensure
			definition: Result = sequence.domain [n]
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

feature -- Cursor movement

	forth
			-- Move cursor to next element.
			-- (from TRAVERSABLE_SUBSET)
		note
			modify: index
		require -- from TRAVERSABLE_SUBSET
			not_after: not after
		deferred
		ensure
			index_effect: index = old index + 1
		end

	go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th item.
		note
			modify: index
		require
			valid_index: valid_index (i)
		deferred
		ensure
			index_effect: index = i
		end

	start
			-- Move cursor to first item.
			-- (from TRAVERSABLE_SUBSET)
		note
			modify: index
		deferred
		ensure
			index_effect: index = 1
		end

feature -- Element change

	extend (v: G)
			-- Ensure that set includes `v'.
			-- Was declared in SET as synonym of put.
			-- (from SET)
		note
			modify: sequence, index
		require
			extendible: extendible
		deferred
		ensure
			set_effect_not_has: not set_has (old set, v) implies set |=| (old set & v)
			set_effect_has: set_has (old set, v) implies set |=| old set
		end

	fill (other: FLAT_LINEAR_SUBSET [G])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current structure
			-- need not be the same.
			-- (from COLLECTION)
		note
			modify: sequence, index
		require -- from COLLECTION
			other_not_void: other /= Void
			extendible: extendible
		local
			lin_rep: FLAT_LINEAR_SUBSET [G]
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
			set_effect_old: (old set).for_all (agent set_has (set, ?))
			set_effect_other: (old other.set).for_all (agent set_has (set, ?))
			set_effect_new: set.for_all (agent (x: G; c, o: MML_SET [G]): BOOLEAN
				do
					Result := set_has (c, x) or set_has (o, x)
				end (?, old set, old other.set))
		end

	merge (other: FLAT_LINEAR_SUBSET [G])
			-- Add all items of `other'.
			-- (from TRAVERSABLE_SUBSET)
		note
			modify: sequence, index
		require -- from SUBSET
			set_exists: other /= Void
			same_rule: object_comparison = other.object_comparison
		local
			l: FLAT_LINEAR_SUBSET [G]
		do
			if attached {FLAT_LINEAR_SUBSET [G]} other as lin_rep then
				l := lin_rep
			else
				l := other.linear_representation
			end
			from
				l.start
			until
				l.off
			loop
				extend (l.item)
				l.forth
			end
		ensure
			set_effect_old: (old set).for_all (agent set_has (set, ?))
			set_effect_other: (old other.set).for_all (agent set_has (set, ?))
			set_effect_new: set.for_all (agent (x: G; c, o: MML_SET [G]): BOOLEAN
				do
					Result := set_has (c, x) or set_has (o, x)
				end (?, old set, old other.set))
		end

	move_item (v: G)
			-- Move `v' to the left of cursor.
		note
			modify: sequence, index
		require
			item_exists: v /= Void
			item_in_set: has (v)
		local
			idx: INTEGER_32
			found: BOOLEAN
		do
			idx := index
			from
				start
			until
				found or after
			loop
				if object_comparison then
					found := v ~ item
				else
					found := (v = item)
				end
				if not found then
					forth
				end
			end
			check
				found: found and not after
			end
			remove
			go_i_th (idx)
			put_left (v)
		ensure
			sequence_effect_has: set_has (old set, v) implies sequence |=|
				old (sequence.removed_at (index_that (sequence, agent element_equal (v, ?))).extended_at (index, v))
			sequence_effect_not_has: not set_has (old set, v) implies sequence |=| old sequence
			index_effect_has: set_has (old set, v) implies index = old index + 1
			index_effect_not_has: not set_has (old set, v) implies index = old index
		end

	put (v: G)
			-- Ensure that set includes `v'.
			-- Was declared in SET as synonym of extend.
			-- (from SET)
		note
			modify: sequence, index
		require
			extendible: extendible
		deferred
		ensure
			set_effect_not_has: not set_has (old set, v) implies set |=| (old set & v)
			set_effect_has: set_has (old set, v) implies set |=| old set
		end

	put_left (v: G)
			-- Insert `v' before the cursor.
		note
			modify: sequence, index
		require
			item_exists: v /= Void
			not_before: not before
		deferred
		ensure
			sequence_effect: sequence |=| old sequence.extended_at (index, v)
			index_effect: index = old index + 1
		end

feature -- Removal

	changeable_comparison_criterion: BOOLEAN
			-- May object_comparison be changed?
			-- (Answer: only if set empty; otherwise insertions might
			-- introduce duplicates, destroying the set property.)
			-- (from SET)
		do
			Result := is_empty
		ensure then
			definition: Result = sequence.is_empty
		end

	prune (v: G)
			-- Remove `v' if present.
			-- (from SET)
		note
			modify: sequence, index
		require
			prunable: prunable
		deferred
		ensure
			set_effect_has: set_has (old set, v) implies set |=| old (set - (set | agent element_equal (v, ?)))
			set_effect_not_has: not set_has (old set, v) implies set |=| old set
		end

	prune_all (v: G)
			-- Remove all occurrences of `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from COLLECTION)
		note
			modify: sequence, index
		require
			prunable: prunable
		do
			from
			until
				not has (v)
			loop
				prune (v)
			end
		ensure
			set_effect_has: set_has (old set, v) implies set |=| old (set - (set | agent element_equal (v, ?)))
			set_effect_not_has: not set_has (old set, v) implies set |=| old set
		end

	remove
			-- Remove current item.
			-- (from TRAVERSABLE_SUBSET)
		note
			modify: sequence
		require
			not_off: not off
		deferred
		ensure
			sequence_effect: sequence |=| old sequence.removed_at (index)
		end

	wipe_out
			-- Remove all items.
			-- (from COLLECTION)
		note
			modify: sequence, index
		require
			prunable: prunable
		deferred
		ensure
			sequence_effect: sequence.is_empty
			index_effect: index = 0
		end

feature -- Conversion

	linear_representation: FLAT_LINEAR_SUBSET [G]
			-- Representation as a linear structure
			-- (from CONTAINER)
		deferred
		ensure
			definition: Result = Current
		end

feature -- Duplication

	duplicate (n: INTEGER_32): FLAT_LINEAR_SUBSET [G]
			-- New structure containing min (`n', count)
			-- items from current structure
			-- (from SUBSET)
		require
			non_negative: n >= 0
		deferred
		ensure
			sequence_definition: Result.sequence |=| sequence.interval (index, index + n - 1)
			object_comparison_definition: not Result.object_comparison
		end

feature -- Basic operations

	intersect (other: FLAT_LINEAR_SUBSET [G])
			-- Remove all items not in `other'.
			-- No effect if `other' is_empty.
			-- (from TRAVERSABLE_SUBSET)
		note
			modify: sequence, index
		require
			set_exists: other /= Void
			same_rule: object_comparison = other.object_comparison
		do
			if not other.is_empty then
				from
					start
					other.start
				until
					off
				loop
					if other.has (item) then
						forth
					else
						remove
					end
				end
			else
				wipe_out
			end
		ensure
			set_effect_old: (old set).for_all (agent (x: G; o: MML_SET [G]): BOOLEAN
				do
					Result := set_has (set, x) = set_has (o, x)
				end (?, old other.set))
			set_effect_new: set.for_all (agent set_has (old set, ?))
		end

	subtract (other: FLAT_LINEAR_SUBSET [G])
			-- Remove all items also in `other'.
			-- (from TRAVERSABLE_SUBSET)
		note
			modify: sequence, index
		require
			set_exists: other /= Void
			same_rule: object_comparison = other.object_comparison
		do
			if not (other.is_empty or is_empty) then
				from
					start
					other.start
				until
					off
				loop
					if other.has (item) then
						remove
					else
						forth
					end
				end
			end
		ensure
			set_effect_old: (old set).for_all (agent (x: G; o: MML_SET [G]): BOOLEAN
				do
					Result := set_has (set, x) or set_has (o, x)
				end (?, old other.set))
			set_effect_new: set.for_all (agent set_has(old set, ?))
		end

	symdif (other: FLAT_LINEAR_SUBSET [G])
			-- Remove all items also in `other', and add all
			-- items of `other' not already present.
			-- (from TRAVERSABLE_SUBSET)
		note
			modify: sequence, index, other_index
		require
			set_exists: other /= Void
			same_rule: object_comparison = other.object_comparison
		local
			s: FLAT_SUBSET_STRATEGY [G]
		do
			if not other.is_empty then
				if is_empty then
					from
						other.start
					until
						other.after
					loop
						extend (other.item)
					end
				else
					s := subset_strategy (other)
					s.symdif (Current, other)
				end
			end
		ensure
			set_effect_old: (old set).for_all (agent (x: G; o: MML_SET [G]): BOOLEAN
				do
					Result := not set_has (o, x) implies set_has (set, x)
				end (?, old other.set))
			set_effect_other: (old other.set).for_all (agent (x: G; c: MML_SET [G]): BOOLEAN
				do
					Result := not set_has (c, x) implies set_has (set, x)
				end (?, old set))
			set_effect_new: set.for_all (agent (x: G; c, o: MML_SET [G]): BOOLEAN
				do
					Result := set_has (c, x) or set_has (o, x)
				end (?, old set, old other.set))
		end

feature {NONE} -- Implementation

	subset_strategy (other: FLAT_LINEAR_SUBSET [G]): FLAT_SUBSET_STRATEGY [G]
			-- Subset strategy suitable for the type of the contained elements.
			-- (from TRAVERSABLE_SUBSET)
		require -- from TRAVERSABLE_SUBSET
			not_empty: not is_empty
		do
			start
			check
				not_off: not off
			end
			Result := subset_strategy_selection (item, other)
		end

	subset_strategy_selection (v: G; other: FLAT_LINEAR_SUBSET [G]): FLAT_SUBSET_STRATEGY [G]
			-- Strategy to calculate several subset features selected depending
			-- on the dynamic type of `v' and `other'
		require -- from TRAVERSABLE_SUBSET
			item_exists: v /= Void
			other_exists: other /= Void
		do
			if attached {HASHABLE} v as h then
				create {FLAT_SUBSET_STRATEGY_HASHABLE [G]} Result
			else
				create {FLAT_SUBSET_STRATEGY_GENERIC [G]} Result
			end
		ensure -- from TRAVERSABLE_SUBSET
			strategy_set: Result /= Void
		end

feature -- Specification

	sequence: MML_SEQUENCE [G]
			-- Sequence of elements (in the order of traversal).
		note
			status: specification
		local
			n: INTEGER
		do
			n := index
			from
				start
				create Result
			until
				after
			loop
				Result := Result & item
				forth
			end
			go_i_th (n)
		end

	set: MML_SET [G]
			-- Set of elements.
		note
			status: specification
		do
			Result := sequence.range
		ensure
			definition: Result |=| sequence.range
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

	set_has (s: MML_SET [G]; v: G): BOOLEAN
			-- Spec implementation of `has'.
		note
			status: specification
		do
			Result := s.exists (agent element_equal (v, ?))
		ensure
			definition: Result = s.exists (agent element_equal (v, ?))
		end

	index_that (s: MML_SEQUENCE [G]; pred: PREDICATE [ANY, TUPLE [G]]): INTEGER
			-- Index of the (only) element of `s' satisfying `pred'.
		note
			status: specification
		require
			exists: s.range.exists (pred)
		do
			Result := s.inverse.restricted (s.range | pred).range.any_item
		ensure
			Result = s.inverse.restricted (s.range | pred).range.any_item
		end

invariant

	no_duplicates: sequence.count = set.count
	index_constraint: 0 <= index and index <= sequence.count + 1

end -- class LINEAR_SUBSET

