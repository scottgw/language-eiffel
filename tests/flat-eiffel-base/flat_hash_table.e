note
	model: map, capacity, object_comparison, conflict, inserted, replaced, removed, found, not_found, found_item, iteration_map, index, off_index

class
	FLAT_HASH_TABLE [G, K -> HASHABLE]

inherit
	V_EQUALITY [G]
		redefine
			copy,
			is_equal
		end

	V_ORDER [INTEGER]
		redefine
			copy,
			is_equal
		end

create
	make

feature -- Initialization

	accommodate (n: INTEGER_32)
			-- Reallocate table with enough space for `n' items;
			-- keep all current items.
		note
			modify: capacity
		require
			n >= 0
		local
			i, nb: INTEGER_32
			new_table: like Current
			l_content: like content
			l_keys: like keys
		do
			from
				new_table := empty_duplicate (keys.count.max (n))
				l_content := content
				l_keys := keys
				nb := l_keys.count
			until
				i = nb
			loop
				if occupied (i) then
					new_table.put (l_content.item (i), l_keys.item (i))
				end
				i := i + 1
			end
			if has_default then
				i := indexes_map.item (capacity)
				new_table.put (l_content.item (i), keys.item (i))
			end
			set_content (new_table.content)
			set_keys (new_table.keys)
			set_deleted_marks (new_table.deleted_marks)
			set_indexes_map (new_table.indexes_map)
			capacity := new_table.capacity
			iteration_position := new_table.iteration_position
		ensure
			capacity_effect: capacity >= n and capacity > count
		end

	make (n: INTEGER_32)
			-- Allocate hash table for at least `n' items.
			-- The table will be resized automatically
			-- if more than `n' items are inserted.
		require
			n_non_negative: n >= 0
		local
			clever: PRIMES
			l_default_value: G
			l_default_key: K
			l_size: INTEGER_32
		do
			create clever
			l_size := n.max (Minimum_capacity)
			l_size := l_size + l_size // 2 + 1
			l_size := clever.higher_prime (l_size)
			capacity := l_size
			create content.make_empty (n + 1)
			create keys.make_empty (n + 1)
			create deleted_marks.make_filled (False, n + 1)
			create indexes_map.make_filled (Ht_impossible_position, l_size + 1)
			iteration_position := n + 1
			count := 0
			deleted_item_position := Ht_impossible_position
			control := 0
			found_item := l_default_value
			has_default := False
			item_position := 0
			ht_lowest_deleted_position := Ht_max_position
			ht_deleted_item := l_default_value
			ht_deleted_key := l_default_key
		ensure
			map_effect: map.is_empty
			capacity_effect:capacity > n
			object_comparison_effect: not object_comparison
			status_definition: not (conflict or inserted or replaced or removed or found or not_found)
			found_item_definition: found_item = ({G}).default
			iteration_map_effect: iteration_map.is_empty
			off_index_effect: off_index = 0
		end

feature -- Access

	at alias "@" (key: K): G assign force
			-- Item associated with `key', if present
			-- otherwise default value of type `G'
			-- Was declared in FLAT_HASH_TABLE as synonym of item.
		require
			valid_key: valid_key (key)
		local
			old_control, old_position: INTEGER_32
		do
			old_control := control
			old_position := item_position
			internal_search (key)
			if found then
				Result := content.item (position)
			end
			control := old_control
			item_position := old_position
		ensure
			definition_has: map_has_key (key) implies Result = map [map_key (key)]
			definition_not_has: not map_has_key (key) implies Result = ({G}).default
		end

	current_keys: FLAT_ARRAY [K]
			-- New array containing actually used keys, from 1 to count
		local
			j: INTEGER_32
			old_iteration_position: INTEGER_32
		do
			if is_empty then
				create Result.make_empty
			else
				old_iteration_position := iteration_position
				from
					start
					create Result.make_filled (key_for_iteration, 1, count)
					j := 1
					forth
				until
					off
				loop
					j := j + 1
					Result.put (key_for_iteration, j)
					forth
				end
				iteration_position := old_iteration_position
			end
		ensure
			map_contraint: Result.map.range |=| map.domain
			object_comparison_definition: not Result.object_comparison
		end

	cursor: FLAT_HASH_TABLE_CURSOR
			-- Current cursor position
		do
			create {FLAT_HASH_TABLE_CURSOR} Result.make (iteration_position)
		ensure
			cursor_not_void: Result /= Void
		end

	found_item: G
			-- Item, if any, yielded by last search operation

	has (key: K): BOOLEAN
			-- Is there an item in the table with key `key'?
		local
			old_control, old_position: INTEGER_32
		do
			old_control := control
			old_position := item_position
			internal_search (key)
			Result := found
			control := old_control
			item_position := old_position
		ensure
			definition: Result = map_has_key (key)
		end

	has_item (v: G): BOOLEAN
			-- Does structure include `v'?
			-- (Reference or object equality,
			-- based on object_comparison.)
		local
			i, nb: INTEGER_32
			l_content: like content
		do
			if has_default then
				Result := (v = content.item (indexes_map.item (capacity)))
			end
			if not Result then
				l_content := content
				nb := l_content.count
				if object_comparison then
					from
					until
						i = nb or else Result
					loop
						Result := occupied (i) and then (v ~ l_content.item (i))
						i := i + 1
					end
				else
					from
					until
						i = nb or else Result
					loop
						Result := occupied (i) and then (v = l_content.item (i))
						i := i + 1
					end
				end
			end
		ensure
			definition: Result = map.range.exists (agent element_equal (v, ?))
		end

	has_key (key: K): BOOLEAN
			-- Is there an item in the table with key `key'? Set found_item to the found item.
		note
			modify: conflict, inserted, replaced, removed, found, not_found, found_item
		local
			old_position: INTEGER_32
			l_default_value: G
		do
			old_position := item_position
			internal_search (key)
			Result := found
			if Result then
				found_item := content.item (position)
			else
				found_item := l_default_value
			end
			item_position := old_position
		ensure
			definition: Result = map_has_key (key)
			found_effect: found = Result
			not_found_effect: not_found = not Result
			found_item_effect_found: Result implies found_item = map [map_key (key)]
			found_item_ffect_not_found: not Result implies found_item = ({G}).default
		end

	item alias "[]" (key: K): G assign force
			-- Item associated with `key', if present
			-- otherwise default value of type `G'
			-- Was declared in FLAT_HASH_TABLE as synonym of at.
		require
			valid_key: valid_key (key)
		local
			old_control, old_position: INTEGER_32
		do
			old_control := control
			old_position := item_position
			internal_search (key)
			if found then
				Result := content.item (position)
			end
			control := old_control
			item_position := old_position
		ensure
			definition_has: map_has_key (key) implies Result = map [map_key (key)]
			definition_not_has: not map_has_key (key) implies Result = ({G}).default
		end

	item_for_iteration: G
			-- Element at current iteration position
		require
			not_off: not off
		do
			Result := content.item (iteration_position)
		ensure
			definition: Result = map [iteration_map [index]]
		end

	new_cursor: FLAT_HASH_TABLE_ITERATION_CURSOR [G, K]
			-- Fresh cursor associated with current structure
		do
			create Result.make (Current)
			Result.start
		ensure
			target_definition: Result.target = Current
			iteration_position_definition_non_empty: not map.is_empty implies Result.iteration_position = lower
			iteration_position_definition_empty: map.is_empty implies Result.iteration_position = 1
			step_definition: Result.step = 1
			is_reversed_definition: not Result.is_reversed
		end

	iteration_item (i: INTEGER_32): G
			-- Entry at position `i'
		do
			Result := content.item (i)
		ensure
			definition: Result = map [iteration_map [i]]
		end

	key_for_iteration: K
			-- Key at current iteration position
		require
			not_off: not off
		do
			Result := keys.item (iteration_position)
		ensure
			definition: Result = iteration_map [index]
		end

feature -- Measurement

	capacity: INTEGER_32
			-- Number of items that may be stored.

	count: INTEGER_32
			-- Number of items in table

	iteration_index_set: FLAT_INTEGER_INTERVAL
			-- Range of acceptable indexes
		do
			create Result.make (next_iteration_position (-1), previous_iteration_position (keys.count))
		ensure
			set_definition: Result.set |=| {MML_INTERVAL} [[lower, upper]]
		end

	occurrences (v: G): INTEGER_32
			-- Number of table items equal to `v'.
		local
			old_iteration_position: INTEGER_32
		do
			old_iteration_position := iteration_position
			if object_comparison then
				from
					start
				until
					off
				loop
					if item_for_iteration ~ v then
						Result := Result + 1
					end
					forth
				end
			else
				from
					start
				until
					off
				loop
					if item_for_iteration = v then
						Result := Result + 1
					end
					forth
				end
			end
			iteration_position := old_iteration_position
		ensure
			definition: Result = occurrences_that (map, agent element_equal (v, ?))
		end

feature -- Comparison

	is_equal (other: like Current): BOOLEAN
			-- Does table contain the same information as `other'?
		do
			Result := keys ~ other.keys and content ~ other.content and (has_default = other.has_default)
		ensure then
			-- Note: does not correspond to the implementation
			definition: Result = (object_comparison = other.object_comparison and
				map.count = other.map.count and
				map.domain.for_all (agent (k: K; o: like Current): BOOLEAN
					do
						Result := o.map_has_key (k) and then element_equal (map [k], o.map [o.map_key (k)])
					end (?, other)))
		end

	same_keys (a_search_key, a_key: K): BOOLEAN
			-- Does `a_search_key' equal to `a_key'?
		require
			valid_search_key: valid_key (a_search_key)
			valid_key: valid_key (a_key)
		do
			Result := a_search_key ~ a_key
		ensure
			definition: Result = (a_search_key ~ a_key)
		end

feature -- Status report

	after: BOOLEAN
			-- Is cursor past last item?
			-- Was declared in FLAT_HASH_TABLE as synonym of off.
		do
			Result := iteration_position >= keys.count
		ensure
			definition: Result = not iteration_map.domain [index]
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

	conflict: BOOLEAN
			-- Did last operation cause a conflict?
		do
			Result := (control = Conflict_constant)
		end

	Extendible: BOOLEAN = False
			-- May new items be added?

	found: BOOLEAN
			-- Did last operation find the item sought?
		do
			Result := (control = Found_constant)
		end

	Full: BOOLEAN = False
			-- Is structure filled to capacity?

	inserted: BOOLEAN
			-- Did last operation insert an item?
		do
			Result := (control = Inserted_constant)
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
			Result := has_item (v)
		ensure
			definition: Result = map.range.exists (agent element_equal (v, ?))
		end

	not_found: BOOLEAN
			-- Did last operation fail to find the item sought?
		do
			Result := (control = Not_found_constant)
		end

	object_comparison: BOOLEAN
			-- Must search operations use equal rather than `='
			-- for comparing references? (Default: no, use `='.)
			-- (from CONTAINER)

	off: BOOLEAN
			-- Is cursor past last item?
			-- Was declared in FLAT_HASH_TABLE as synonym of after.
		do
			Result := iteration_position >= keys.count
		ensure
			definition: Result = not iteration_map.domain [index]
		end

	prunable: BOOLEAN
			-- May items be removed?
		do
			Result := True
		ensure
			definition: Result
		end

	removed: BOOLEAN
			-- Did last operation remove an item?
		do
			Result := (control = Removed_constant)
		end

	replaced: BOOLEAN
			-- Did last operation replace an item?
		do
			Result := (control = Replaced_constant)
		end

	valid_cursor (c: FLAT_HASH_TABLE_CURSOR): BOOLEAN
			-- Can cursor be moved to position `c'?
		require
			c_not_void: c /= Void
		do
			if attached {FLAT_HASH_TABLE_CURSOR} c as ht_cursor then
				Result := valid_iteration_index (ht_cursor.position)
			end
		end

	valid_iteration_index (i: INTEGER_32): BOOLEAN
			-- Is `i' a valid index?
		do
			Result := (is_off_position (i)) or else ((i >= 0) and (i <= keys.count) and then truly_occupied (i))
		ensure
			definition: Result = (i > off_index or iteration_map.domain [i])
		end

	valid_key (k: K): BOOLEAN
			-- Is `k' a valid key?
		local
			l_internal: INTERNAL
			l_default_key: K
			l_index, i, nb: INTEGER_32
			l_name: STRING_8
			l_cell: CELL [K]
		do
			Result := True
			debug ("prevent_hash_table_catcall")
				if k /= l_default_key then
					create l_internal
					create l_cell.put (l_default_key)
					from
						i := 1
						nb := l_internal.field_count (l_cell)
						l_name := "item"
					until
						i > nb
					loop
						if l_internal.field_name (i, l_cell) ~ l_name then
							l_index := i
							i := nb + 1
						end
						i := i + 1
					end
					if l_index > 0 and then k /= Void then
						Result := l_internal.field_static_type_of_type (l_index, l_internal.dynamic_type (l_cell)) = l_internal.dynamic_type (k)
					end
				end
			end
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

feature -- Cursor movement

	forth
			-- Advance cursor to next occupied position,
			-- or off if no such position remains.
		note
			modify: index
		require
			not_off: not off
		do
			iteration_position := next_iteration_position (iteration_position)
		ensure
			index_efect: index > old index and {MML_INTERVAL} [[old index + 1, index - 1]].disjoint (iteration_map.domain)
		end

	go_to (c: FLAT_HASH_TABLE_CURSOR)
			-- Move to position `c'.
		note
			modify: index
		require
			c_not_void: c /= Void
			valid_cursor: valid_cursor (c)
		do
			if attached {FLAT_HASH_TABLE_CURSOR} c as ht_cursor then
				iteration_position := ht_cursor.position
			end
		end

	search (key: K)
			-- Search for item of key `key'.
			-- If found, set found to true, and set
			-- found_item to item associated with `key'.
		note
			modify: conflict, inserted, replaced, removed, found, not_found, found_item
		local
			old_position: INTEGER_32
			l_default_value: G
		do
			old_position := item_position
			internal_search (key)
			if found then
				found_item := content.item (position)
			else
				found_item := l_default_value
			end
			item_position := old_position
		ensure
			found_effect: map_has_key (key) implies found
			not_found_effect: not map_has_key (key) implies not_found
			found_item_effect_found: map_has_key (key) implies found_item = map [map_key (key)]
			found_item_effect_not_found: not map_has_key (key) implies found_item = ({G}).default
		end

	start
			-- Bring cursor to first position.
		note
			modify: index
		do
			iteration_position := -1
			forth
		ensure
			index_effect: index = lower
		end

feature {FLAT_HASH_TABLE_ITERATION_CURSOR} -- Cursor movement

	next_iteration_position (a_position: like iteration_position): like iteration_position
			-- Given an iteration position, advanced to the next one taking into account deleted
			-- slots in the content and keys structures.
		require
			a_position_big_enough: a_position >= -1
			a_position_small_enough: a_position < keys.count
		local
			l_deleted_marks: like deleted_marks
			l_table_size: INTEGER_32
		do
			Result := a_position + 1
			l_deleted_marks := deleted_marks
			l_table_size := content.count
			from
			until
				Result >= l_table_size or else not l_deleted_marks.item (Result)
			loop
				Result := Result + 1
			end
		end

	previous_iteration_position (a_position: like iteration_position): like iteration_position
			-- Given an iteration position, go to the previous one taking into account deleted
			-- slots in the content and keys structures.
		require
			a_position_big_enough: a_position >= 0
			a_position_small_enough: a_position <= keys.count
		local
			l_deleted_marks: like deleted_marks
			l_table_size: INTEGER_32
		do
			Result := a_position - 1
			l_deleted_marks := deleted_marks
			l_table_size := content.count
			from
			until
				Result <= 0 or else not l_deleted_marks.item (Result)
			loop
				Result := Result - 1
			end
		end

feature -- Element change

	extend (new: G; key: K)
			-- Assuming there is no item of key `key',
			-- insert `new' with `key'.
			-- Set inserted.
			--
			-- To choose between various insert/replace procedures,
			-- see `instructions' in the Indexing clause.
		note
			modify: map, capacity, iteration_map, off_index, inserted, replaced, removed, found, not_found
		require
			not_present: not has (key)
		local
			l_default_key: K
			l_new_pos, l_new_index_pos: like position
		do
			search_for_insertion (key)
			if soon_full then
				add_space
				search_for_insertion (key)
			end
			if deleted_item_position /= Ht_impossible_position then
				l_new_pos := deleted_position (deleted_item_position)
				l_new_index_pos := deleted_item_position
				deleted_marks.force (False, l_new_pos)
			else
				l_new_pos := keys.count
				l_new_index_pos := item_position
			end
			indexes_map.put (l_new_pos, l_new_index_pos)
			content.force (new, l_new_pos)
			keys.force (key, l_new_pos)
			if key = l_default_key then
				has_default := True
			end
			count := count + 1
			control := Inserted_constant
		ensure
			map_effect: map |=| old map.updated (key, new)
			capacity_effect: capacity >= old capacity
			off_index_effect: off_index >= old off_index
			inserted_effect: inserted
		end

	fill (other: FLAT_HASH_TABLE [G, K])
			-- Fill with as many items of `other' as possible.
			-- The representations of `other' and current structure
			-- need not be the same.
			-- (from COLLECTION)
		require
			other_not_void: other /= Void
			extendible: Extendible
		local
			lin_rep: FLAT_DYNAMIC_LIST [G]
		do
			lin_rep := other.linear_representation
			from
				lin_rep.start
			until
				not Extendible or else lin_rep.off
			loop
				collection_extend (lin_rep.item)
				lin_rep.forth
			end
			-- ToDo			
		end

	force (new: G; key: K)
			-- Update table so that `new' will be the item associated
			-- with `key'.
			-- If there was an item for that key, set found
			-- and set found_item to that item.
			-- If there was none, set not_found and set
			-- found_item to the default value.
			--
			-- To choose between various insert/replace procedures,
			-- see `instructions' in the Indexing clause.
		note
			modify: map, capacity, iteration_map, off_index, inserted, replaced, removed, found, not_found, found_item
		require
			valid_key: valid_key (key)
		local
			l_default_key: K
			l_default_value: G
			l_new_pos, l_new_index_pos: like position
		do
			internal_search (key)
			if not_found then
				if soon_full then
					add_space
					internal_search (key)
				end
				if deleted_item_position /= Ht_impossible_position then
					l_new_pos := deleted_position (deleted_item_position)
					l_new_index_pos := deleted_item_position
					deleted_marks.force (False, l_new_pos)
				else
					l_new_pos := keys.count
					l_new_index_pos := item_position
				end
				indexes_map.put (l_new_pos, l_new_index_pos)
				keys.force (key, l_new_pos)
				if key = l_default_key then
					has_default := True
				end
				count := count + 1
				found_item := l_default_value
			else
				l_new_pos := position
				found_item := content.item (l_new_pos)
			end
			content.force (new, l_new_pos)
		ensure
			map_effect_has: old map_has_key (key) implies map |=| old map.updated (map_key (key), new)
			map_effect_not_has: not old map_has_key (key) implies map |=| old map.updated (key, new)
			capacity_effect: capacity >= old capacity
			capacity_effect_has: old map_has_key (key) implies capacity = old capacity
			off_index_effect: off_index >= old off_index
			off_index_effect_has: old map_has_key (key) implies off_index = old off_index
			iteration_map_effect_has: old map_has_key (key) implies iteration_map |=| old iteration_map
			found_effect_has: found = old map_has_key (key)
			not_found_effect_not_has: not_found = not old map_has_key (key)
			found_item_effect_has: old map_has_key (key) implies found_item = map [map_key (key)]
			found_item_effect_not_has: not old map_has_key (key) implies found_item = ({G}).default
		end

	merge (other: FLAT_HASH_TABLE [G, K])
			-- Merge `other' into Current. If `other' has some elements
			-- with same key as in `Current', replace them by one from
			-- `other'.
		note
			modify: map, capacity, iteration_map, off_index, inserted, replaced, removed, found, not_found, found_item, other_index
		require
			other_not_void: other /= Void
		do
			from
				other.start
			until
				other.after
			loop
				force (other.item_for_iteration, other.key_for_iteration)
				other.forth
			end
		ensure
			map_domain_effect: map.domain |=| old (other.map.domain - (other.map.domain | agent map_has_key) + map.domain)
			map_effect_other: map.domain.for_all (agent (k: K; m1, m2: MML_MAP [K, G]): BOOLEAN
				do
					Result := (m2.domain.exists (agent same_keys (?, k)) implies map [k] = m2 [(m2.domain | agent same_keys (?, k)).any_item]) and
						(not m2.domain.exists (agent same_keys (?, k)) implies map [k] = m1 [k])
				end (?, old map, old other.map))
			capacity_effect: capacity >= old capacity
			off_index_effect: off_index >= old off_index
		end

	put (new: G; key: K)
			-- Insert `new' with `key' if there is no other item
			-- associated with the same key.
			-- Set inserted if and only if an insertion has
			-- been made (i.e. `key' was not present).
			-- If so, set position to the insertion position.
			-- If not, set conflict.
			-- In either case, set found_item to the item
			-- now associated with `key' (previous item if
			-- there was one, `new' otherwise).
			--
			-- To choose between various insert/replace procedures,
			-- see `instructions' in the Indexing clause.
		note
			modify: map, capacity, iteration_map, off_index, inserted, replaced, removed, found, not_found, found_item
		require
			valid_key: valid_key (key)
		local
			l_default_key: K
			l_new_pos, l_new_index_pos: like position
		do
			internal_search (key)
			if found then
				set_conflict
				found_item := content.item (position)
			else
				if soon_full then
					add_space
					internal_search (key)
					check
						not_present: not found
					end
				end
				if deleted_item_position /= Ht_impossible_position then
					l_new_pos := deleted_position (deleted_item_position)
					l_new_index_pos := deleted_item_position
					deleted_marks.force (False, l_new_pos)
				else
					l_new_pos := keys.count
					l_new_index_pos := item_position
				end
				indexes_map.put (l_new_pos, l_new_index_pos)
				content.force (new, l_new_pos)
				keys.force (key, l_new_pos)
				if key = l_default_key then
					has_default := True
				end
				count := count + 1
				found_item := new
				control := Inserted_constant
			end
		ensure
			map_effect_has: old map_has_key (key) implies map |=| old map
			map_effect_not_has: not old map_has_key (key) implies map |=| old map.updated (key, new)
			capacity_effect: capacity >= old capacity
			capacity_effect_has: old map_has_key (key) implies capacity = old capacity
			off_index_effect: off_index >= old off_index
			off_index_effect_has: old map_has_key (key) implies off_index = old off_index
			iteration_map_effect_has: old map_has_key (key) implies iteration_map |=| old iteration_map
			conflict_effect_has: conflict = old map_has_key (key)
			insertion_effect_not_has: inserted = not old map_has_key (key)
			found_item_effect: found_item = map [map_key (key)]
		end

	replace (new: G; key: K)
			-- Replace item at `key', if present,
			-- with `new'; do not change associated key.
			-- Set replaced if and only if a replacement has been made
			-- (i.e. `key' was present); otherwise set not_found.
			-- Set found_item to the item previously associated
			-- with `key' (default value if there was none).
			--
			-- To choose between various insert/replace procedures,
			-- see `instructions' in the Indexing clause.
		note
			modify: map, inserted, replaced, removed, found, not_found, found_item
		local
			l_default_item: G
		do
			internal_search (key)
			if found then
				found_item := content.item (position)
				content.put (new, position)
				control := Replaced_constant
			else
				found_item := l_default_item
			end
		ensure
			map_effect_has: old map_has_key (key) implies map |=| old map.updated (map_key (key), new)
			map_effect_not_has: not old map_has_key (key) implies map |=| old map
			replaced_effect_has: replaced = old map_has_key (key)
			not_found_effect_not_has: not_found = not old map_has_key (key)
			found_item_effect_has: old map_has_key (key) implies found_item = old map [map_key (key)]
			found_item_effect_not_has: not old map_has_key (key) implies found_item = ({G}).default
		end

	replace_key (new_key: K; old_key: K)
			-- If there is an item of key `old_key' and no item of key
			-- `new_key', replace the former's key by `new_key',
			-- set replaced, and set found_item to the item
			-- previously associated with `old_key'.
			-- Otherwise set not_found or conflict respectively.
			-- If conflict, set found_item to the item previously
			-- associated with `new_key'.
			--
			-- To choose between various insert/replace procedures,
			-- see `instructions' in the Indexing clause.
		note
			modify: map, iteration_map, inserted, replaced, removed, found, not_found, found_item
		local
			l_item: G
		do
			internal_search (new_key)
			if not found then
				internal_search (old_key)
				if found then
					l_item := content.item (position)
					remove (old_key)
					put (l_item, new_key)
					control := Replaced_constant
				end
			else
				set_conflict
				found_item := content.item (position)
			end
		ensure
			map_effect_ok: old map_has_key (old_key) and not old map_has_key (new_key) implies
				map |=| old (map.updated (new_key, map [map_key (old_key)]).removed (map_key (old_key)))
			map_effect_not_ok: not old map_has_key (old_key) or old map_has_key (new_key) implies map |=| old map
			replaced_effect_has: replaced = (old map_has_key (old_key) and not old map_has_key (new_key))
			conflict_effect_not_has: conflict = old map_has_key (new_key)
			not_found_effect_not_has: not_found = (not old map_has_key (old_key) and not old map_has_key (new_key))
			found_item_effect_has_new: old map_has_key (new_key) implies found_item = old map [map_key (new_key)]
			found_item_effect_has_old: not old map_has_key (new_key) and old map_has_key (old_key) implies found_item = map [map_key (new_key)]
			found_item_effect_not_has: not old map_has_key (new_key) and not old map_has_key (old_key) implies found_item = old found_item
		end

feature -- Removal

	prune (v: G)
			-- Remove first occurrence of `v', if any,
			-- after cursor position.
			-- Move cursor to right neighbor.
			-- (or after if no right neighbor or `v' does not occur)
		note
			modify: map, index, iteration_map, inserted, replaced, removed, found, not_found, found_item
		require
			prunable: prunable
		do
			if object_comparison then
				from
				until
					after or else item_for_iteration ~ v
				loop
					forth
				end
			else
				from
				until
					after or else item_for_iteration = v
				loop
					forth
				end
			end
			if not after then
				remove (key_for_iteration)
			end
		ensure
			map_effect_has: not old (indexes_that (agent element_equal (v, ?)).disjoint ({MML_INTERVAL} [[index, upper]])) implies
				map |=| old map.removed (iteration_map [(indexes_that (agent element_equal (v, ?)) * {MML_INTERVAL} [[index, upper]]).extremum (agent less_equal)])
			map_effect_not_has: old (indexes_that (agent element_equal (v, ?)).disjoint ({MML_INTERVAL} [[index, upper]])) implies
				map |=| old map
			removed_effect: removed = not old (indexes_that (agent element_equal (v, ?)).disjoint ({MML_INTERVAL} [[index, upper]]))
			not_found_effect: not_found = not removed
			found_item_effect_has: removed implies found_item = ({G}).default
			found_item_effect_not_has: not removed implies found_item = old found_item
			index_effect_has: removed implies
				index = old (indexes_that (agent element_equal (v, ?)) * {MML_INTERVAL} [[index, upper]]).extremum (agent less_equal)
			index_effect_not_has: not removed implies
				index = off_index
		end

	remove (key: K)
			-- Remove item associated with `key', if present.
			-- Set removed if and only if an item has been
			-- removed (i.e. `key' was present);
			-- if so, set position to index of removed element.
			-- If not, set not_found.
			-- Reset found_item to its default value if removed.
		note
			modify: map, iteration_map, inserted, replaced, removed, found, not_found, found_item
		local
			l_default_key: K
			l_default_value: G
			l_pos: like position
			l_nb_removed_items: INTEGER_32
		do
			internal_search (key)
			if found then
				l_pos := position
				if key = l_default_key then
					has_default := False
				end
				deleted_marks.put (True, l_pos)
				indexes_map.put (- l_pos + Ht_deleted_position, item_position)
				if iteration_position = l_pos then
					forth
				end
				count := count - 1
				ht_lowest_deleted_position := l_pos.min (ht_lowest_deleted_position)
				if (ht_lowest_deleted_position = count) then
					l_nb_removed_items := content.count - ht_lowest_deleted_position
					content.remove_tail (l_nb_removed_items)
					keys.remove_tail (l_nb_removed_items)
					deleted_marks.fill_with (False, ht_lowest_deleted_position, deleted_marks.count - 1)
					ht_deleted_item := l_default_value
					ht_deleted_key := l_default_key
					ht_lowest_deleted_position := Ht_max_position
				elseif attached ht_deleted_item as l_item and attached ht_deleted_key as l_key then
					content.put (l_item, l_pos)
					keys.put (l_key, l_pos)
				else
					ht_deleted_item := content.item (l_pos)
					ht_deleted_key := keys.item (l_pos)
				end
				control := Removed_constant
				found_item := l_default_value
			end
		ensure
			map_effect_has: old map_has_key (key) implies map |=| old map.removed (map_key (key))
			map_effect_not_has: not old map_has_key (key) implies map |=| old map
			removed_effect: removed = old map_has_key (key)
			not_found_effect: not_found = not old map_has_key (key)
			found_item_effect_has: old map_has_key (key) implies found_item = ({G}).default
			found_item_effect_not_has: not old map_has_key (key) implies found_item = old found_item
		end

	wipe_out
			-- Reset all items to default values; reset status.
		note
			modify: map, conflict, inserted, replaced, removed, found, not_found, found_item, iteration_map, index, off_index
		require
			prunable: prunable
		local
			l_default_value: G
		do
			content.wipe_out
			keys.wipe_out
			deleted_marks.fill_with (False, 0, deleted_marks.upper)
			indexes_map.fill_with (Ht_impossible_position, 0, capacity)
			found_item := l_default_value
			count := 0
			item_position := 0
			iteration_position := keys.count
			control := 0
			has_default := False
		ensure
			map_effect: map.is_empty
			status_effect: not (conflict or inserted or replaced or removed or found or not_found)
			found_item_effect: found_item = ({G}).default
			iteration_map_effect: iteration_map.is_empty
			index_effect: index = 0
			off_index_effect: off_index = 0
		end

feature {NONE} -- Removal

	prune_all (v: G)
			-- Remove all occurrences of `v'.
			-- (Reference or object equality,
			-- based on object_comparison.)
			-- (from COLLECTION)
		require -- from COLLECTION
			prunable: prunable
		do
			from
			until
				not has_item (v)
			loop
				prune (v)
			end
		ensure -- from COLLECTION
			no_more_occurrences: not has_item (v)
		end

feature -- Conversion

	linear_representation: FLAT_ARRAYED_LIST [G]
			-- Representation as a linear structure
		local
			old_iteration_position: INTEGER_32
		do
			old_iteration_position := iteration_position
			from
				create Result.make (count)
				start
			until
				off
			loop
				Result.extend (item_for_iteration)
				forth
			end
			iteration_position := old_iteration_position
		ensure
			sequence_definition: Result.sequence |=| map.sequence_image (to_sequence (iteration_map))
			object_comparison_deifnition: not Result.object_comparison
		end

feature -- Duplication

	copy (other: like Current)
			-- Re-initialize from `other'.
		note
			modify: map, capacity, object_comparison, conflict, inserted, replaced, removed, found, not_found, found_item, iteration_map, index, off_index
		do
			if other /= Current then
				standard_copy (other)
				set_content (other.content.twin)
				set_keys (other.keys.twin)
				set_deleted_marks (other.deleted_marks.twin)
				set_indexes_map (other.indexes_map.twin)
			end
		ensure then
			map_effect: map |=| other.map
			capacity_effect: capacity = other.capacity
			object_comparison_effect: object_comparison = other.object_comparison
			conflict_effect: conflict = other.conflict
			inserted_effect: inserted = other.inserted
			replaced_effect: replaced = other.replaced
			removed_effect: removed = other.removed
			found_effect: found = other.found
			not_found_effect: not_found = other.not_found
			found_item_effect: found_item = other.found_item
			iteration_map_effect: iteration_map |=| other.iteration_map
			index_effect: index = other.index
			off_index_effect: off_index = other.off_index
		end

feature {NONE} -- Duplication

	empty_duplicate (n: INTEGER_32): like Current
			-- Create an empty copy of Current that can accommodate `n' items
		require
			n_non_negative: n >= 0
		do
			create Result.make (n)
		ensure
			empty_duplicate_attached: Result /= Void
		end

feature {NONE} -- Inapplicable

	bag_put (v: G)
			-- Ensure that structure includes `v'.
			-- (from TABLE)
		require -- from COLLECTION
			extendible: Extendible
		do
		ensure -- from COLLECTION
			item_inserted: is_inserted (v)
		end

	collection_extend (v: G)
			-- Insert a new occurrence of `v'.
		require -- from COLLECTION
			extendible: Extendible
		do
		ensure -- from COLLECTION
			item_inserted: is_inserted (v)
		end

feature {NONE} -- Implementation

	add_space
			-- Increase capacity.
		do
			accommodate (count + count // 2)
		ensure
			count_not_changed: count = old count
			breathing_space: count < capacity
		end

	computed_default_key: K
			-- Default key
			-- (For performance reasons, used only in assertions;
			-- elsewhere, see use of local entity `l_default_key'.)
		do
		end

	computed_default_value: G
			-- Default value of type G
			-- (For performance reasons, used only in assertions;
			-- elsewhere, see use of local entity `l_default_value'.)
		do
		end

	Conflict_constant: INTEGER_32 = 1
			-- Could not insert an already existing key

	default_key_value: G
			-- Value associated with the default key, if any
		require
			has_default: has_default
		do
			Result := content [indexes_map [capacity]]
		end

	deleted (i: INTEGER_32): BOOLEAN
			-- Is position `i' that of a deleted item?
		require
			in_bounds: i >= 0 and i <= capacity
		do
			Result := indexes_map.item (i) <= Ht_deleted_position
		end

	deleted_position (a_pos: INTEGER_32): INTEGER_32
			-- Given the position of a deleted item at `a_pos' gives the associated position
			-- in `content/keys'.
		require
			deleted: deleted (a_pos)
		do
			Result := - indexes_map.item (a_pos) + Ht_deleted_position
			Result := Result.min (keys.count)
		ensure
			deleted_position_non_negative: Result >= 0
			deleted_position_valid: Result <= keys.count and Result <= content.count
		end

	Found_constant: INTEGER_32 = 2
			-- Key found

	ht_deleted_item: G

	ht_deleted_key: K
			-- Store the item and key that will be used to replace an element of the FLAT_HASH_TABLE
			-- that will be removed. If elements being removed are at the end of content or keys
			-- then they are both Void. It is only used when removing an element at a position strictly
			-- less than count.

	Ht_deleted_position: INTEGER_32 = -2
			-- Marked a deleted position.

	Ht_impossible_position: INTEGER_32 = -1
			-- Position outside the array indices.

	ht_lowest_deleted_position: INTEGER_32
			-- Index of the lowest deleted position thus far.

	Ht_max_position: INTEGER_32 = 2147483645
			-- Maximum possible position

	initial_position (hash_value: INTEGER_32): INTEGER_32
			-- Initial position for an item of hash code `hash_value'
		do
			Result := (hash_value \\ capacity)
		end

	Inserted_constant: INTEGER_32 = 4
			-- Insertion successful

	internal_search (key: K)
			-- Search for item of key `key'.
			-- If successful, set position to index
			-- of item with this key (the same index as the key's index).
			-- If not, set position to possible position for insertion,
			-- and set status to found or not_found.
		local
			l_default_key: K
			hash_value, increment, l_pos, l_item_pos, l_capacity: INTEGER_32
			l_first_deleted_position: INTEGER_32
			stop: INTEGER_32
			l_keys: like keys
			l_indexes: like indexes_map
			l_deleted_marks: like deleted_marks
			l_key: K
		do
			l_first_deleted_position := Ht_impossible_position
			if key = l_default_key or key = Void then
				item_position := capacity
				if has_default then
					control := Found_constant
				else
					control := Not_found_constant
				end
			else
				from
					l_keys := keys
					l_indexes := indexes_map
					l_deleted_marks := deleted_marks
					l_capacity := capacity
					stop := l_capacity
					hash_value := key.hash_code
					increment := 1 + hash_value \\ (l_capacity - 1)
					l_item_pos := (hash_value \\ l_capacity) - increment
					control := Not_found_constant
				until
					stop = 0
				loop
					l_item_pos := (l_item_pos + increment) \\ l_capacity
					l_pos := l_indexes [l_item_pos]
					if l_pos >= 0 then
						l_key := l_keys.item (l_pos)
						debug ("detect_hash_table_catcall")
							check
								catcall_detected: l_key /= Void and then l_key.same_type (key)
							end
						end
						if same_keys (l_key, key) then
							stop := 1
							control := Found_constant
						end
					elseif l_pos = Ht_impossible_position then
						stop := 1
					elseif l_first_deleted_position = Ht_impossible_position then
						l_pos := - l_pos + Ht_deleted_position
						check
							l_pos_valid: l_pos < l_deleted_marks.count
						end
						if not l_deleted_marks [l_pos] then
							stop := 1
						else
							l_first_deleted_position := l_item_pos
						end
					end
					stop := stop - 1
				end
				item_position := l_item_pos
			end
			deleted_item_position := l_first_deleted_position
		ensure
			found_or_not_found: found or not_found
			deleted_item_at_deleted_position: (deleted_item_position /= Ht_impossible_position) implies (deleted (deleted_item_position))
			default_iff_at_capacity: (item_position = capacity) = (key = computed_default_key)
		end

	is_off_position (pos: INTEGER_32): BOOLEAN
			-- Is `pos' a cursor position past last item?
		do
			Result := pos >= keys.count
		end

	key_at (n: INTEGER_32): K
			-- Key at position `n'
		do
			if keys.valid_index (n) then
				Result := keys.item (n)
			end
		end

	Minimum_capacity: INTEGER_32 = 2

	Not_found_constant: INTEGER_32 = 8
			-- Key not found

	occupied (i: INTEGER_32): BOOLEAN
			-- Is position `i' occupied by a non-default key and a value?
		require
			in_bounds: deleted_marks.valid_index (i)
		do
			if has_default then
				Result := i /= indexes_map.item (capacity) and then not deleted_marks.item (i)
			else
				Result := not deleted_marks.item (i)
			end
		end

	position_increment (hash_value: INTEGER_32): INTEGER_32
			-- Distance between successive positions for hash code
			-- `hash_value' (computed for no cycle: capacity is prime)
		do
			Result := 1 + hash_value \\ (capacity - 1)
		end

	Removed_constant: INTEGER_32 = 16
			-- Remove successful

	Replaced_constant: INTEGER_32 = 32
			-- Replaced value

	search_for_insertion (key: K)
			-- Assuming there is no item of key `key', compute
			-- position at which to insert such an item.
		require
			not_present: not has (key)
		local
			l_default_key: K
			hash_value, increment, l_pos, l_item_pos, l_capacity: INTEGER_32
			l_first_deleted_position: INTEGER_32
			stop: INTEGER_32
			l_keys: like keys
			l_indexes: like indexes_map
			l_deleted_marks: like deleted_marks
		do
			l_first_deleted_position := Ht_impossible_position
			if key = l_default_key or key = Void then
				check
					not has_default
				end
				item_position := capacity
			else
				from
					l_keys := keys
					l_indexes := indexes_map
					l_deleted_marks := deleted_marks
					l_capacity := capacity
					stop := l_capacity
					hash_value := key.hash_code
					increment := 1 + hash_value \\ (l_capacity - 1)
					l_item_pos := (hash_value \\ l_capacity) - increment
				until
					stop = 0
				loop
					l_item_pos := (l_item_pos + increment) \\ l_capacity
					l_pos := l_indexes [l_item_pos]
					if l_pos >= 0 then
					elseif l_pos = Ht_impossible_position then
						stop := 1
					elseif l_first_deleted_position = Ht_impossible_position then
						l_pos := - l_pos + Ht_deleted_position
						check
							l_pos_valid: l_pos < l_deleted_marks.count
						end
						if not l_deleted_marks [l_pos] then
							stop := 1
						else
							l_first_deleted_position := l_item_pos
						end
					end
					stop := stop - 1
				end
				item_position := l_item_pos
			end
			deleted_item_position := l_first_deleted_position
		ensure
			deleted_item_at_deleted_position: (deleted_item_position /= Ht_impossible_position) implies (deleted (deleted_item_position))
			default_iff_at_capacity: (item_position = capacity) = (key = computed_default_key)
		end

	set_conflict
			-- Set status to conflict.
		do
			control := Conflict_constant
		ensure
			conflict: conflict
		end

	set_content (c: like content)
			-- Assign `c' to content.
		require
			c_attached: c /= Void
		do
			content := c
		ensure
			content_set: content = c
		end

	set_deleted_marks (d: like deleted_marks)
			-- Assign `c' to content.
		require
			d_attached: d /= Void
		do
			deleted_marks := d
		ensure
			deleted_marks_set: deleted_marks = d
		end

	set_found
			-- Set status to found.
		do
			control := Found_constant
		ensure
			found: found
		end

	set_indexes_map (v: like indexes_map)
			-- Assign `v' to indexes_map.
		do
			indexes_map := v
		ensure
			indexes_map_set: indexes_map = v
		end

	set_inserted
			-- Set status to inserted.
		do
			control := Inserted_constant
		ensure
			inserted: inserted
		end

	set_keys (c: like keys)
			-- Assign `c' to keys.
		require
			c_attached: c /= Void
		do
			keys := c
		ensure
			keys_set: keys = c
		end

	set_no_status
			-- Set status to normal.
		do
			control := 0
		ensure
			default_status: not special_status
		end

	set_not_found
			-- Set status to not found.
		do
			control := Not_found_constant
		ensure
			not_found: not_found
		end

	set_removed
			-- Set status to removed.
		do
			control := Removed_constant
		ensure
			removed: removed
		end

	set_replaced
			-- Set status to replaced.
		do
			control := Replaced_constant
		ensure
			replaced: replaced
		end

	special_status: BOOLEAN
			-- Has status been set to some non-default value?
		do
			Result := (control > 0)
		ensure
			Result = (conflict or inserted or replaced or removed or found or not_found)
		end

	truly_occupied (i: INTEGER_32): BOOLEAN
			-- Is position `i' occupied by a key and a value?
		do
			if i >= 0 and i < keys.count then
				Result := (has_default and i = indexes_map.item (capacity)) or else occupied (i)
			end
		ensure
			normal_key: (i >= 0 and i < keys.count and i /= indexes_map.item (capacity)) implies (occupied (i) implies Result)
			default_key: (i = indexes_map.item (capacity)) implies (Result = has_default)
		end

feature {FLAT_HASH_TABLE, FLAT_HASH_TABLE_ITERATION_CURSOR} -- Implementation: content attributes and preservation

	content: SPECIAL [G]
			-- Array of contents

	keys: SPECIAL [K]
			-- Array of keys

feature {FLAT_HASH_TABLE} -- Implementation: content attributes and preservation

	deleted_marks: SPECIAL [BOOLEAN]
			-- Indexes of deleted positions in content and keys.

	has_default: BOOLEAN
			-- Is the default key present?

	indexes_map: SPECIAL [INTEGER_32]
			-- Indexes of items in content, and keys.
			-- If item is not present, then it has `ht_mpossible_position'.
			-- If item is deleted, then it has Ht_deleted_position.

	item_position: INTEGER_32
			-- Position in indexes_map for item at position position. Set by internal_search.

feature {FLAT_HASH_TABLE} -- Implementation: search attributes

	control: INTEGER_32
			-- Control code set by operations that may produce
			-- several possible conditions.

	deleted_item_position: INTEGER_32
			-- Place where a deleted element was found during a search

	iteration_position: INTEGER_32
			-- Cursor for iteration primitives

	position: INTEGER_32
			-- Hash table cursor, updated after each operation:
			-- put, remove, has, replace, force, change_key...
		do
			Result := indexes_map.item (item_position)
		end

	soon_full: BOOLEAN
			-- Is table close to being filled to current capacity?
		do
			Result := keys.count = keys.capacity
		ensure
			Result = (keys.count = keys.capacity)
		end

feature -- Specification

	map: MML_MAP [K, G]
			-- Map from keys to values
		note
			status: specification
		local
			c: like new_cursor
		do
			from
				c := new_cursor
				create Result
			until
				c.after
			loop
				Result := Result.updated (c.key, c.item)
				c.forth
			end
		end

	map_has_key (k: K): BOOLEAN
			-- Does `map' contain `k'? (Use object comparison)
		note
			status: specification
		do
			Result := map.domain.exists (agent same_keys (?, k))
		ensure
			Result = map.domain.exists (agent same_keys (?, k))
		end

	map_key (k: K): K
			-- Key in `map' equivalent to `k' according to object comparison.
		note
			status: specification
		require
			has_key: map_has_key (k)
		do
			Result := (map.domain | agent same_keys (?, k)).any_item
		ensure
			Result = (map.domain | agent same_keys (?, k)).any_item
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

	iteration_map: MML_MAP [INTEGER, K]
			-- Map of valid indexes to key.
		note
			status: specification
		local
			c: like new_cursor
		do
			from
				c := new_cursor
				create Result
			until
				c.after
			loop
				Result := Result.updated (c.iteration_position, c.key)
				c.forth
			end
		end

	lower: INTEGER
			-- First iteration index.
		note
			status: specification
		do
			if map.is_empty then
				Result := 0
			else
				Result := iteration_map.domain.extremum (agent less_equal)
			end
		ensure
			definition_empty: map.is_empty implies Result = 0
			definition_nonempty: not map.is_empty implies Result = iteration_map.domain.extremum (agent less_equal)
		end

	upper: INTEGER
			-- Last iteration index.
		note
			status: specification
		do
			if map.is_empty then
				Result := -1
			else
				Result := iteration_map.domain.extremum (agent greater_equal)
			end
		ensure
			definition_empty: map.is_empty implies Result = -1
			definition_nonempty: not map.is_empty implies Result = iteration_map.domain.extremum (agent greater_equal)
		end

	index: INTEGER
			-- Cursor position.
		note
			status: specification
		do
			Result := iteration_position
		end

	off_index: INTEGER
			-- Lowest cursor position when it's off.
		do
			Result := keys.count
		end

	occurrences_that (m: MML_MAP [K, G]; pred: PREDICATE [ANY, TUPLE [G]]): INTEGER
			-- Number of elements of `m' that satisfy `pred'.
		note
			status: specification
		do
			Result := m.inverse.restricted (m.range | pred).count
		ensure
			definition: Result = m.inverse.restricted (m.range | pred).count
		end

	indexes_that (pred: PREDICATE [ANY, TUPLE [G]]): MML_SET [INTEGER]
			-- Indexes of keys who's value satisfies `pred'.
		note
			status: specification
		do
			Result := iteration_map.inverse.image (map.inverse.image (map.range | pred))
		ensure
			definition: Result |=| iteration_map.inverse.image (map.inverse.image (map.range | pred))
		end

	to_sequence (m: MML_MAP [INTEGER, K]): MML_SEQUENCE [K]
			-- Sequence of elements in `m'.
		note
			status: specification
		local
			l: INTEGER
		do
			if m.is_empty then
				create Result
			else
				l := m.domain.extremum (agent less_equal)
				Result := to_sequence (m.removed (l)).prepended (m [l])
			end
		ensure
			definition_empty: m.is_empty implies Result.is_empty
			definition_not_empty: not m.is_empty implies Result =
				to_sequence (m.removed (m.domain.extremum (agent less_equal))).prepended (m [m.domain.extremum (agent less_equal)])
		end

invariant
	capacity_constraint: capacity >= map.count
	iteration_map_domain_constraint: iteration_map.domain <= {MML_INTERVAL} [[0, capacity]]
	iteration_map_range_constraint: iteration_map.count = map.count and iteration_map.range |=| map.domain
	off_index_constraint_not_empty: off_index > upper
	index_constraint: index >= off_index or iteration_map.domain [index]
	conflict_exclusive: conflict implies not (inserted or replaced or removed or found or not_found)
	inserted_exclusive: inserted implies not (conflict or replaced or removed or found or not_found)
	replaced_exclusive: replaced implies not (conflict or inserted or removed or found or not_found)
	removed_exclusive: removed implies not (conflict or inserted or replaced or found or not_found)
	found_exclusive: found implies not (conflict or inserted or replaced or removed or not_found)
	not_found_exclusive: not_found implies not (conflict or inserted or replaced or removed or found)

	count_definition: count = map.count

	keys_not_void: keys /= Void
	content_not_void: content /= Void
	keys_enough_capacity: keys.count <= capacity + 1
	content_enough_capacity: content.count <= capacity + 1
	valid_iteration_position: off or truly_occupied (iteration_position)
	control_non_negative: control >= 0
	special_status: special_status = (conflict or inserted or replaced or removed or found or not_found)

end -- class FLAT_HASH_TABLE

