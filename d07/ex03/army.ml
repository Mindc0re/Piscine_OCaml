class ['a] army =
	object

		val _army_members : 'a list = []

		method add member = {< _army_members = (List.append _army_members [member]); >}

		method delete = {<
							_army_members = match _army_members with 
											| hd::tl -> tl
											| [] -> []
		 				>}

		method get_army = _army_members

	end