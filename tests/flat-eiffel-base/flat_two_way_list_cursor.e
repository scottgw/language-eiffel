class 
	FLAT_TWO_WAY_LIST_CURSOR [G]
	
inherit
	FLAT_CURSOR

create 
	make

feature {NONE} -- Initialization

	make (active_element: like active; aft, bef: BOOLEAN)
			-- Create a cursor and set it up on `active_element'.
			-- (from LINKED_LIST_CURSOR)
		do
			active := active_element
			after := aft
			before := bef
		end
	
feature {FLAT_TWO_WAY_LIST, FLAT_TWO_WAY_SORTED_SET} -- Implementation

	active: FLAT_BI_LINKABLE [G]
			-- Current element in linked list
	
	after: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?
			-- (from LINKED_LIST_CURSOR)

	before: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?
			-- (from LINKED_LIST_CURSOR)
	
invariant
		-- from LINKED_LIST_CURSOR
	not_both: not (before and after)
	no_active_not_on: active = Void implies (before or after)

end -- class TWO_WAY_LIST_CURSOR

