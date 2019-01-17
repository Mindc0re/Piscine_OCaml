class people name =
	object
		initializer print_endline ("People with name " ^ name ^ " has been created")

		val _name:string 	= name
		val _hp:int 		= 100

		method to_string 	= _name ^ " : " ^ string_of_int _hp ^ " HP"
		method talk 		= print_endline ("I’m " ^ _name ^ "! Do you know the Doctor?")
		method die 			= print_endline "Aaaarghh!"
	end
