deferred class
	FLAT_SUBSET_STRATEGY [G]

feature -- Comparison

	disjoint (set1, set2: FLAT_LINEAR_SUBSET [G]): BOOLEAN
			-- Are `set1' and `set2' disjoint?
		require
			sets_exist: set1 /= Void and set2 /= Void
			same_rule: set1.object_comparison = set2.object_comparison
		deferred
		ensure
			definition: Result = not set1.set.exists (agent set1.set_has (set2.set, ?))
		end

feature -- Basic operations

	symdif (set1, set2: FLAT_LINEAR_SUBSET [G])
			-- Remove all items of `set1' that are also in `set2', and add all
			-- items of `set2' not already present in `set1'.
		note
			modifies: set1_sequence, set1_index, set2_index
		require
			sets_exist: set1 /= Void and set2 /= Void
			same_rule: set1.object_comparison = set2.object_comparison
		deferred
		ensure
			set_effect_old: (old set1.set).for_all (agent (x: G; s: MML_SET [G]; c: FLAT_LINEAR_SUBSET [G]): BOOLEAN
				do
					Result := not c.set_has (s, x) implies c.set_has (c.set, x)
				end (?, old set2.set, set1))
			set_effect_other: (old set2.set).for_all (agent (x: G; s: MML_SET [G]; c: FLAT_LINEAR_SUBSET [G]): BOOLEAN
				do
					Result := not c.set_has (s, x) implies c.set_has (c.set, x)
				end (?, old set1.set, set1))
			set_effect_new: set1.set.for_all (agent (x: G; s1, s2: MML_SET [G]; c: FLAT_LINEAR_SUBSET [G]): BOOLEAN
				do
					Result := c.set_has (s1, x) or c.set_has (s2, x)
				end (?, old set1.set, old set2.set, set1))
		end

end -- class FLAT_SUBSET_STRATEGY
