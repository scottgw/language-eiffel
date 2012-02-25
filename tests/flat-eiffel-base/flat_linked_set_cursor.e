class
	FLAT_LINKED_SET_CURSOR [G]

inherit
	FLAT_CURSOR

create
	make

feature {NONE} -- Initialization

	make (active_element: like active; aft, bef: BOOLEAN)
			-- Create a cursor and set it up on `active_element'.
		do
			active := active_element
			after := aft
			before := bef
		end

feature {FLAT_LINKED_SET} -- Implementation

	active: FLAT_LINKABLE [G]
			-- Current element in linked list

	after: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?

	before: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?


invariant
	not_both: not (before and after)
	no_active_not_on: active = Void implies (before or after)

end -- class LINKED_LIST_CURSOR

