class FLAT_SUBSET_STRATEGY_GENERIC [G] inherit

	FLAT_SUBSET_STRATEGY [G]

feature -- Comparison

	disjoint (set1, set2: FLAT_LINEAR_SUBSET [G]): BOOLEAN
			-- Are `set1' and `set2' disjoint?
		local
			tmp_set: FLAT_LINKED_SET [G]
		do
			create tmp_set.make
			if set1.object_comparison then
				tmp_set.compare_objects
			end
			from
				Result := True
				set1.start
				set2.start
			until
				not Result or else (set1.after and set2.after)
			loop
				if not set1.after then
					Result := not tmp_set.has (set1.item)
					if Result then tmp_set.put (set1.item) end
				end
				if Result and then not set2.after then
					Result := not tmp_set.has (set2.item)
					if Result then tmp_set.put (set2.item) end
				end
				set1.forth
				set2.forth
			end
		end

feature -- Basic operations

	symdif (set1, set2: FLAT_LINEAR_SUBSET [G])
			-- Remove all items of `set1' that are also in `set2', and add all
			-- items of `set2' not already present in `set1'.
		local
			tmp_set: FLAT_LINKED_LIST [G]
		do
			create tmp_set.make
			if set1.object_comparison then
				tmp_set.compare_objects
			end
			from set1.start until set1.after loop
				tmp_set.extend (set1.item)
				set1.forth
			end
			from set2.start until set2.after loop
				if tmp_set.has (set2.item) then
					tmp_set.prune (set2.item)
				else
					tmp_set.extend (set2.item)
				end
				set2.forth
			end
			set1.wipe_out
			from tmp_set.start until tmp_set.after loop
				set1.extend (tmp_set.item)
				tmp_set.forth
			end
		end

end -- class FLAT_SUBSET_STRATEGY_GENERIC
