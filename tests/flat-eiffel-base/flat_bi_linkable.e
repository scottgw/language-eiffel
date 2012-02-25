note
	model: item, left, right

class
	FLAT_BI_LINKABLE [G]

create {FLAT_TWO_WAY_LIST, FLAT_TWO_WAY_SORTED_SET}
	put

feature -- Access

	item: G
			-- Content of cell.
			-- (from CELL)

	left: like Current
			-- Left neighbor

	right: like Current
			-- Right neighbor
			-- (from LINKABLE)

feature {FLAT_BI_LINKABLE, FLAT_TWO_WAY_LIST, FLAT_TWO_WAY_SORTED_SET} -- Element change

	put (v: like item)
			-- Make `v' the cell's item.
			-- Was declared in CELL as synonym of replace.
			-- (from CELL)
		note
			modify: item
		do
			item := v
		ensure
			item_effect: item = v
		end

feature -- Element change

	replace (v: like item)
			-- Make `v' the cell's item.
			-- Was declared in CELL as synonym of put.
			-- (from CELL)
		note
			modify: item
		do
			item := v
		ensure -- from CELL
			item_inserted: item = v
		end

feature {FLAT_BI_LINKABLE, FLAT_TWO_WAY_LIST, FLAT_TWO_WAY_SORTED_SET} -- Implementation

	forget_left
			-- Remove links with left neighbor.
		note
			modify: left, old_left_right
		local
			l: like left
		do
			l := left
			if l /= Void then
				l.simple_forget_right
				left := Void
			end
		ensure
			left_effect: left = Void
			old_left_right_effect: (old left /= Void) implies (old left).right = Void
		end

	forget_right
			-- Remove links with right neighbor.
		note
			modify: right, old_right_left
		local
			l_right: like right
		do
			l_right := right
			if l_right /= Void then
				l_right.simple_forget_left
				right := Void
			end
		ensure
			right_effect: right = Void
			old_right_left_effect: (old right /= Void) implies (old right).left = Void
		end

	put_left (other: like Current)
			-- Put `other' to the left of current cell.
		note
			modify: left, old_left_right, other_right, old_other_right_left
		local
			l: like left
		do
			l := left
			if l /= Void then
				l.simple_forget_right
			end
			left := other
			if other /= Void then
				other.simple_put_right (Current)
			end
		ensure
			left_effect: left = other
			left_right_effect: (old left /= Void) implies (old left).right = Void
			other_right_effect: other /= Void implies other.right = Current
			other_right_left_effect: other /= Void and then (old other.right) /= Void implies (old other.right).left = Void
		end

	put_right (other: like Current)
			-- Put `other' to the right of current cell.
		note
			modify: right, old_right_left, other_left, old_other_left_right
		local
			l_right: like right
			l_other: like other
		do
			l_right := right
			if l_right /= Void then
				l_right.simple_forget_left
			end
			right := other
			l_other := other
			if l_other /= Void then
				l_other.simple_put_left (Current)
			end
		ensure
			right_effect: right = other
			right_left_effect: (old right /= Void) implies (old right).left = Void
			other_left_effect: other /= Void implies other.left = Current
			other_left_right_effect: other /= Void and then (old other.left) /= Void implies (old other.left).right = Void
		end

feature {FLAT_BI_LINKABLE, FLAT_TWO_WAY_LIST, FLAT_TWO_WAY_SORTED_SET} -- Implementation

	simple_forget_left
			-- Remove left link (do nothing to left neighbor).
		note
			modify: left
		do
			left := Void
		ensure
			left_effect: left = Void
		end

	simple_forget_right
			-- Remove right link (do nothing to right neighbor).
		note
			modify: right
		do
			right := Void
		ensure
			right_effect: right = Void
		end

	simple_put_left (other: like Current)
			-- Set left to `other' is
		note
			modify: left, old_left_right
		local
			l: like left
		do
			l := left
			if l /= Void then
				l.simple_forget_right
			end
			left := other
		ensure
			left_effect: left = other
			old_left_right_effect: (old left /= Void) implies (old left).right = Void
		end

	simple_put_right (other: like Current)
			-- Set right to `other'
		note
			modify: right, old_right_left
		local
			l_right: like right
		do
			l_right := right
			if l_right /= Void then
				l_right.simple_forget_left
			end
			right := other
		ensure
			right_effect: right = other
			old_right_left_effect: (old right /= Void) implies (old right).left = Void
		end

invariant

	right_symmetry: attached right as r implies (r.left = Current)
	left_symmetry: attached left as l implies (l.right = Current)

end -- class BI_LINKABLE

