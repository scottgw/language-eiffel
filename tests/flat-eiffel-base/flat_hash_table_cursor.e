class FLAT_HASH_TABLE_CURSOR

create
	make

feature {NONE} -- Initialization

	make (pos: INTEGER)
			-- Create a new cursor.
		do
			position := pos
		ensure
			position_set: position = pos
		end

feature {FLAT_HASH_TABLE} -- Access

	position: INTEGER
			-- Cursor position

end -- class HASH_TABLE_CURSOR


