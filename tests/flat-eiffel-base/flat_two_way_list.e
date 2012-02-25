note
	model: sequence, index, object_comparison, sublist

class
	FLAT_TWO_WAY_LIST [G]

inherit
	FLAT_DYNAMIC_LIST [G]
		redefine
			first,
			last,
			new_cursor,
			after,
			before,
			is_inserted,
			isfirst,
			islast,
			off,
			readable,
			finish,
			go_i_th,
			move,
			start,
			merge_left,
			merge_right,
			put_left,
			wipe_out,
			copy
		end

create
	make


create {FLAT_TWO_WAY_LIST}
	make_sublist

feature {NONE} -- Initialization

	make
			-- Create an empty list.
			-- (from LINKED_LIST)
		do
			before := True
		ensure
			sequence_effect: sequence.is_empty
			index_effect: index = 0
			object_comparison_effect: not object_comparison
			sublist_effect: sublist = Void
		end

feature -- Access

	cursor: FLAT_TWO_WAY_LIST_CURSOR [G]
			-- Current cursor position
		do
			create Result.make (active, after, before)
		end

	first: like item
			-- Item at first position
			-- (from LINKED_LIST)
		local
			f: like first_element
		do
			f := first_element
			check
				f_attached: f /= Void
			end
			Result := f.item
		end

	index: INTEGER_32
			-- Index of current position
			-- (from LINKED_LIST)
		local
			l_active, l_active_iterator: like active
		do
			if after then
				Result := count + 1
			elseif not before then
				from
					Result := 1
					l_active := active
					l_active_iterator := first_element
				until
					l_active_iterator = l_active or else l_active_iterator = Void
				loop
					l_active_iterator := l_active_iterator.right
					Result := Result + 1
				end
			end
		end

	item: G
			-- Current item
			-- (from LINKED_LIST)
		local
			a: like active
		do
			a := active
			check
				a_attached: a /= Void
			end
			Result := a.item
		end

	last: like item
			-- Item at last position
			-- (from LINKED_LIST)
		local
			l: like last_element
		do
			l := last_element
			check
				l_attached: l /= Void
			end
			Result := l.item
		end

	new_cursor: FLAT_TWO_WAY_LIST_ITERATION_CURSOR [G]
			-- Fresh cursor associated with current structure
		do
			create Result.make (Current)
			Result.start
		end

	sublist: like Current
			-- Result produced by last split

feature -- Measurement

	count: INTEGER_32
			-- Number of items
			-- (from LINKED_LIST)

feature -- Status report

	after: BOOLEAN
			-- Is there no valid cursor position to the right of cursor?
			-- (from LINKED_LIST)

	before: BOOLEAN
			-- Is there no valid cursor position to the left of cursor?
			-- (from LINKED_LIST)

	Full: BOOLEAN = False
			-- Is structured filled to capacity? (Answer: no.)
			-- (from LINKED_LIST)

	is_inserted (v: G): BOOLEAN
			-- Has `v' been inserted at the end by the most recent put or
			-- extend?
			-- (from LINKED_LIST)
		local
			l: like last_element
		do
			l := last_element
			if l /= Void then
				check
					put_constraint: (v /= l.item) implies not off
				end
				Result := (v = l.item) or else (v = item)
			end
		end

	isfirst: BOOLEAN
			-- Is cursor at first position?
			-- (from LINKED_LIST)
		do
			Result := not after and not before and (active = first_element)
		end

	islast: BOOLEAN
			-- Is cursor at last position?
		do
			Result := (active = last_element) and then not after and then not before
		end

	off: BOOLEAN
			-- Is there no current item?
			-- (from LINKED_LIST)
		do
			Result := after or before
		end

	readable: BOOLEAN
			-- Is there a current item that may be read?
			-- (from LINKED_LIST)
		do
			Result := not off
		end

	valid_cursor (p: FLAT_CURSOR): BOOLEAN
			-- Can the cursor be moved to position `p'?
			-- (from LINKED_LIST)
		local
			temp, sought: like first_element
		do
			if attached {like cursor} p as ll_c then
				from
					temp := first_element
					sought ?= ll_c.active
					Result := ll_c.after or else ll_c.before
				until
					Result or else temp = Void
				loop
					Result := (temp = sought)
					temp := temp.right
				end
			end
		end

feature -- Cursor movement

	back
			-- Move cursor to previous position, if any.
		local
			a: like active
		do
			if after then
				after := False
				if is_empty then
					before := True
				end
			else
				a := active
				if a /= Void then
					a := a.left
					if a = Void then
						active := first_element
						before := True
					else
						active := a
					end
				end
			end
		end

	finish
			-- Move cursor to last position.
			-- (Go before if empty)
		do
			if not is_empty then
				active := last_element
				after := False
				before := False
			else
				after := False
				before := True
			end
		end

	forth
			-- Move cursor to next position, if any.
		local
			a: like active
		do
			if before then
				before := False
				if is_empty then
					after := True
				end
			else
				a := active
				if a /= Void then
					a := a.right
					if a = Void then
						active := last_element
						after := True
					else
						active := a
					end
				end
			end
		end

	go_i_th (i: INTEGER_32)
			-- Move cursor to `i'-th position.
			-- (from LINKED_LIST)
		do
			if i = 0 then
				before := True
				after := False
				active := first_element
			elseif i = count + 1 then
				before := False
				after := True
				active := last_element
			else
				move (i - index)
			end
		end

	go_to (p: FLAT_CURSOR)
			-- Move cursor to position `p'.
			-- (from LINKED_LIST)
		do
			if attached {like cursor} p as ll_c then
				after := ll_c.after
				before := ll_c.before
				if before then
					active := first_element
				elseif after then
					active := last_element
				else
					active ?= ll_c.active
				end
			else
				check
					correct_cursor_type: False
				end
			end
		end

	move (i: INTEGER_32)
			-- Move cursor `i' positions. The cursor
			-- may end up off if the offset is to big.
		local
			counter: INTEGER_32
			p: like first_element
		do
			if i > 0 then
				ll_move (i)
			elseif i < 0 then
				if after then
					after := False
					counter := -1
				end
				from
					p := active
				until
					(counter = i) or else (p = Void)
				loop
					p := p.left
					counter := counter - 1
				end
				if p = Void then
					before := True
					active := first_element
				else
					active := p
				end
			end
		end

	start
			-- Move cursor to first position.
			-- (from LINKED_LIST)
		do
			if first_element /= Void then
				active := first_element
				after := False
			else
				after := True
			end
			before := False
		end

feature {NONE} -- Cursor movement

	ll_move (i: INTEGER_32)
			-- Move cursor `i' positions. The cursor
			-- may end up off if the offset is too big.
			-- (from LINKED_LIST)
		local
			counter, new_index: INTEGER_32
			p: like first_element
		do
			if i > 0 then
				if before then
					before := False
					counter := 1
				end
				from
					p := active
				until
					(counter = i) or else (p = Void)
				loop
					active := p
					p := p.right
					counter := counter + 1
				end
				if p = Void then
					after := True
				else
					active := p
				end
			elseif i < 0 then
				new_index := index + i
				before := True
				after := False
				active := first_element
				if (new_index > 0) then
					move (new_index)
				end
			end
		end

feature -- Element change

	extend (v: like item)
			-- Add `v' to end.
			-- Do not move cursor.
		local
			p: like first_element
		do
			p := new_cell (v)
			if is_empty then
				first_element := p
				active := p
			else
				p.put_left (last_element)
			end
			last_element := p
			if after then
				active := p
			end
			count := count + 1
		end

	merge_left (other: like Current)
			-- Merge `other' into current structure before cursor
			-- position. Do not move cursor. Empty `other'.
		local
			other_first_element: like first_element
			other_last_element: like first_element
			other_count: INTEGER_32
			a: like active
		do
			if not other.is_empty then
				other_first_element := other.first_element
				other_last_element := other.last_element
				other_count := other.count
				other.wipe_out
				check
					other_first_element /= Void
					other_last_element /= Void
				end
				if is_empty then
					last_element := other_last_element
					first_element := other_first_element
					if before then
						active := first_element
					else
						active := last_element
					end
				elseif isfirst then
					other_last_element.put_right (first_element)
					first_element := other_first_element
				elseif after then
					other_first_element.put_left (last_element)
					last_element := other_last_element
					active := last_element
				else
					a := active
					if a /= Void then
						other_first_element.put_left (a.left)
						a.put_left (other_last_element)
					end
				end
				count := count + other_count
			end
		end

	merge_right (other: like Current)
			-- Merge `other' into current structure after cursor
			-- position. Do not move cursor. Empty `other'.
		do
			if is_empty or else islast then
				last_element := other.last_element
			end
			ll_merge_right (other)
		end

	put_front (v: like item)
			-- Add `v' to beginning.
			-- Do not move cursor.
		do
			ll_put_front (v)
			if count = 1 then
				last_element := first_element
			end
		end

	put_left (v: like item)
			-- Add `v' to the left of cursor position.
			-- Do not move cursor.
		local
			p: like first_element
			a: like active
		do
			p := new_cell (v)
			if is_empty then
				first_element := p
				last_element := p
				active := p
				before := False
			elseif after then
				p.put_left (last_element)
				last_element := p
				active := p
			elseif isfirst then
				p.put_right (active)
				first_element := p
			else
				a := active
				if a /= Void then
					p.put_left (a.left)
				end
				p.put_right (active)
			end
			count := count + 1
		end

	put_right (v: like item)
			-- Add `v' to the right of cursor position.
			-- Do not move cursor.
		local
			was_last: BOOLEAN
			a: like active
		do
			was_last := islast
			ll_put_right (v)
			if count = 1 then
				last_element := active
			elseif was_last then
				a := active
				if a /= Void then
					last_element := a.right
				end
			end
		end

	replace (v: like item)
			-- Replace current item by `v'.
			-- (from LINKED_LIST)
		local
			a: like active
		do
			a := active
			if a /= Void then
				a.put (v)
			end
		end

feature {NONE} -- Element change

	ll_merge_right (other: FLAT_TWO_WAY_LIST [G])
			-- Merge `other' into current structure after cursor
			-- position. Do not move cursor. Empty `other'.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: extendible
			not_after: not after
			other_exists: other /= Void
			not_current: other /= Current
		local
			other_first_element: like first_element
			other_last_element: like last_element
			other_count: INTEGER_32
			a: like active
		do
			other_last_element := other.last_element
			if other_last_element /= Void then
				other_first_element := other.first_element
				other_count := other.count
				other.wipe_out
				check
					other_first_element /= Void
					other_last_element /= Void
				end
				a := active
				if a = Void then
					first_element := other_first_element
					active := first_element
				else
					if not islast then
						other_last_element.put_right (a.right)
					end
					a.put_right (other_first_element)
				end
				count := count + other_count
			end
		end

	ll_put_front (v: like item)
			-- Add `v' to beginning.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: extendible
		local
			p: like new_cell
		do
			p := new_cell (v)
			p.put_right (first_element)
			first_element := p
			if before or is_empty then
				active := p
			end
			count := count + 1
		end

	ll_put_right (v: like item)
			-- Add `v' to the right of cursor position.
			-- Do not move cursor.
			-- (from LINKED_LIST)
		require -- from DYNAMIC_CHAIN
			extendible: extendible
			not_after: not after
		local
			p: like new_cell
			a: like active
		do
			p := new_cell (v)
			check
				is_empty implies before
			end
			if before then
				p.put_right (first_element)
				first_element := p
				active := p
			else
				a := active
				if a /= Void then
					p.put_right (a.right)
					a.put_right (p)
				end
			end
			count := count + 1
		end

feature -- Removal

	remove
			-- Remove current item.
			-- Move cursor to right neighbor
			-- (or after if no right neighbor).
		local
			succ, pred, removed: like first_element
			a: like active
		do
			removed := active
			if isfirst then
				a := first_element
				if a /= Void then
					active := a.right
					a.forget_right
				end
				first_element := active
				if count = 1 then
					check
						no_active: active = Void
					end
					after := True
					last_element := Void
				end
			elseif islast then
				a := last_element
				if a /= Void then
					active := a.left
					a.forget_left
				end
				last_element := active
				after := True
			else
				a := active
				if a /= Void then
					pred := a.left
					succ := a.right
				end
				if pred /= Void and then succ /= Void then
					pred.forget_right
					succ.forget_left
					pred.put_right (succ)
				end
				active := succ
			end
			count := count - 1
			cleanup_after_remove (removed)
		end

	remove_left
			-- Remove item to the left of cursor position.
			-- Do not move cursor.
		do
			back
			remove
		end

	remove_right
			-- Remove item to the right of cursor position.
			-- Do not move cursor.
		do
			forth
			remove
			back
		end

	remove_sublist
		note
			modify: sublist
		do
			sublist := Void
		ensure
			sublist_effect: sublist = Void
		end

	split (n: INTEGER_32)
			-- Remove from current list
			-- min (`n', count - index - 1) items
			-- starting at cursor position.
			-- Move cursor right one position.
			-- Make extracted sublist accessible
			-- through attribute sublist.
		note
			modify: sequence, sublist, index
		require
			not_off: not off
			valid_sublist: n >= 0
		local
			actual_number, active_index: INTEGER_32
			p_elem, s_elem, e_elem, n_elem: like first_element
		do
			if n = 0 then
				create sublist.make
			else
				active_index := index
				if active_index + n > count + 1 then
					actual_number := count + 1 - active_index
				else
					actual_number := n
				end
				s_elem := active
				p_elem := previous
				move (actual_number - 1)
				e_elem := active
				n_elem := next
				if s_elem /= Void then
					s_elem.forget_left
				end
				if e_elem /= Void then
					e_elem.forget_right
				end
				create sublist.make_sublist (s_elem, e_elem, actual_number)
				count := count - actual_number
				if p_elem /= Void then
					p_elem.put_right (n_elem)
				else
					first_element := n_elem
				end
				if n_elem /= Void then
					active := n_elem
				else
					last_element := p_elem
					active := p_elem
					after := True
				end
			end
		ensure
			sequence_effect: sequence |=| old sequence.removed ({MML_INTERVAL} [[index, index + n - 1]])
			sublist_sequence_effect: sublist.sequence |=| old sequence.restricted ({MML_INTERVAL} [[index, index + n - 1]])
			sublist_object_comparison_effect: not sublist.object_comparison
			sublist_sublist_effect: sublist.sublist = Void
			index_effect: index = (sequence.count + 1).min (old index)
		end

	wipe_out
			-- Remove all items.
		do
			ll_wipe_out
			last_element := Void
		end

feature {NONE} -- Removal

	ll_wipe_out
			-- Remove all items.
			-- (from LINKED_LIST)
		require -- from COLLECTION
			prunable: prunable
		do
			internal_wipe_out
		end

feature -- Duplication

	copy (other: FLAT_TWO_WAY_LIST [G])
			-- Update current object using fields of object attached
			-- to `other', so as to yield equal objects.
			-- (from LINKED_LIST)
		note
			modify: sequence, index, object_comparison, sublist
		local
			cur: FLAT_TWO_WAY_LIST_CURSOR [G]
		do
			if other /= Current then
				standard_copy (other)
				if not other.is_empty then
					internal_wipe_out
					if attached {FLAT_TWO_WAY_LIST_CURSOR [G]} other.cursor as l_cur then
						cur := l_cur
					end
					from
						other.start
					until
						other.off
					loop
						extend (other.item)
						finish
						other.forth
					end
					if cur /= Void then
						other.go_to (cur)
					end
				end
			end
		ensure then
			sequence_effect: sequence |=| other.sequence
			object_comparison_effect: object_comparison = other.object_comparison
			sublist_effect: sublist = other.sublist
		end

feature {FLAT_TWO_WAY_LIST} -- Implementation

	make_sublist (first_item, last_item: like first_element; n: INTEGER_32)
			-- Create sublist
		do
			make
			first_element := first_item
			last_element := last_item
			active := first_element
			count := n
		end

	new_cell (v: like item): FLAT_BI_LINKABLE [G]
			-- A newly created instance of the type of first_element.
		do
			create Result.put (v)
		end

	new_chain: like Current
			-- A newly created instance of the same type.
			-- This feature may be redefined in descendants so as to
			-- produce an adequately allocated and initialized object.
		do
			create Result.make
		end

	previous: like first_element
			-- Element left of cursor
		local
			a: like active
		do
			if after then
				Result := active
			else
				a := active
				if a /= Void then
					Result := a.left
				end
			end
		end

feature {FLAT_TWO_WAY_LIST, FLAT_TWO_WAY_LIST_ITERATION_CURSOR} -- Implementation

	first_element: like new_cell
			-- Head of list
			-- (Anchor redefinition)

	last_element: like first_element
			-- Tail of the list			

	active: like first_element
			-- Element at cursor position
			-- (from LINKED_LIST)

	cleanup_after_remove (v: like first_element)
			-- Clean-up a just removed cell.
			-- (from LINKED_LIST)
		require -- from LINKED_LIST
			non_void_cell: v /= Void
		do
		end

	next: like first_element
			-- Element right of cursor
			-- (from LINKED_LIST)
		local
			a: like active
		do
			if before then
				Result := active
			else
				a := active
				if a /= Void then
					Result := a.right
				end
			end
		end

feature {NONE} -- Implementation

	frozen internal_wipe_out
			-- Remove all items.
			-- (from LINKED_LIST)
		require -- from LINKED_LIST
			prunable
		do
			active := Void
			first_element := Void
			before := True
			after := False
			count := 0
		end

invariant

	count_definition: count = sequence.count
	after_definition: after = (index = sequence.count + 1)
	before_definition: before = (index = 0)

end -- class TWO_WAY_LIST

