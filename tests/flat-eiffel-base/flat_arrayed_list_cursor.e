class
	FLAT_ARRAYED_LIST_CURSOR

inherit
	FLAT_CURSOR

create
	make

feature {NONE} -- Initialization

	make (current_index: INTEGER_32)
			-- Create a cursor and set it up on `current_index'.
		do
			index := current_index
		end

feature {FLAT_ARRAYED_LIST, FLAT_ARRAYED_SET, FLAT_BINARY_TREE} -- Access

	index: INTEGER_32
			-- Index of current item

end -- class FLAT_ARRAYED_LIST_CURSOR

