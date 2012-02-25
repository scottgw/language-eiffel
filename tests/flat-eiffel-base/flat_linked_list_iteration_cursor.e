note
	model: target, target_index, step, is_reversed

class
	FLAT_LINKED_LIST_ITERATION_CURSOR [G]

inherit
	FLAT_INDEXABLE_ITERATION_CURSOR [G]
		redefine
			start,
			item,
			after,
			forth,
			target
		end

create
	make

feature -- Access

	target: FLAT_LINKED_LIST [G]
			-- <Precursor>

	item: G
			-- <Precursor>
		local
			l_active: like active
		do
			if is_reversed then
				Result := Precursor
			else
				l_active := active
					-- Required because `start' sets `active' and `active' can become detached
					-- when falling off the end of the list.
				check l_active_attached: attached l_active end
				Result := l_active.item
			end
		end

feature -- Status report

	after: BOOLEAN
			-- <Precursor>
		do
			if is_reversed then
				Result := Precursor
			else
				Result := not is_valid or active = Void
			end
		end

feature -- Cursor movement

	start
			-- <Precursor>
		do
			Precursor
			if not is_reversed then
				active := target.first_element
			end
		end

	forth
			-- <Precursor>
		local
			i: like step
		do
			Precursor
			if not is_reversed then
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

feature {NONE} -- Implementation

	active: detachable FLAT_LINKABLE [G]
			-- Currrently active linkable node for ascending traversal

end
