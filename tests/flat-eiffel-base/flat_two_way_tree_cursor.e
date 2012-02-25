class 
	FLAT_TWO_WAY_TREE_CURSOR [G]
	
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
	
feature {FLAT_TWO_WAY_TREE} -- Access

	active: FLAT_TWO_WAY_TREE [G]
			-- Current node
	
feature {FLAT_TWO_WAY_TREE} -- Implementation

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

end -- class TWO_WAY_TREE_CURSOR

