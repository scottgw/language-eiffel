note
	model: target, target_index, step, is_reversed

class
	FLAT_TWO_WAY_LIST_ITERATION_CURSOR [G]

inherit
	FLAT_INDEXABLE_ITERATION_CURSOR [G]
		redefine
			item,
			after,
			forth,
			start,
			target
		end

create
	make

feature -- Access

	target: FLAT_TWO_WAY_LIST [G]
			-- Associated structure used for iteration.

	item: G
			-- Item at current cursor position.
		local
			l_active: like active
		do
			l_active := active
			check
				l_active_attached: attached l_active
			end
			Result := l_active.item
		end

feature -- Status report

	after: BOOLEAN
			-- Are there no more items to iterate over?
		do
			Result := not is_valid or active = Void
		end

feature -- Cursor movement

	forth
			-- Move to next position.
		local
			i: like step
		do
			if is_reversed then
				target_index := target_index - step
				from
					i := 1
				until
					i > step or else not attached active as l_active
				loop
					active := l_active.left
					i := i + 1
				end
			else
				target_index := target_index + step
				from
					i := 1
				until
					i > step or else not attached active as l_active
				loop
					active := l_active.right
					i := i + 1
				end
			end
		end

	start
		local
			l_list: like target
		do
			if is_reversed then
				target_index := index_set.upper
			else
				target_index := index_set.lower
			end
			l_list := target
			if is_reversed then
				active := l_list.last_element
			else
				active := l_list.first_element
			end
		end

feature {NONE} -- Implementation

	active: FLAT_BI_LINKABLE [G]
			-- Currrently active linkable node for ascending traversal

end -- class TWO_WAY_LIST_ITERATION_CURSOR

