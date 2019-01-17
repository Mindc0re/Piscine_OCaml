class doctor name =
	object
		initializer print_endline ("Doctor with name " ^ name ^ " has been created")

		val 		_name:string 	= name
		val _sidekick:People.people = new People.people ("Rose Tyler")
		val mutable _hp:int 		= 100
		val mutable _age:int 		= 0

		method to_string 	= ("Doctor " ^ _name ^ ", age : " ^ string_of_int _age ^ ", HP : " ^ string_of_int _hp ^ ", with sidekick : " ^ _sidekick#to_string)
		method talk 		= print_endline ("Hi! I’m the Doctor!")

		method use_sonic_screwdriver 	= print_endline ("Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii")
		method private regenerate 		= _hp <- 100

		method travel_in_time start arrival = _age <- (arrival - start) ; print_endline ("        ___        \n" ^ "_______(_@_)_______\n" ^ "| POLICE      BOX |\n" ^ "|_________________|\n" ^ " | _____ | _____ |\n" ^
" | |###| | |###| |\n" ^ " | |###| | |###| |\n" ^ " | _____ | _____ |\n" ^ " | || || | || || |\n" ^ " | ||_|| | ||_|| |\n" ^
" | _____ |$_____ |\n" ^ " | || || | || || |\n" ^ " | ||_|| | ||_|| |\n" ^ " | _____ | _____ |\n" ^ " | || || | || || | \n" ^
" | ||_|| | ||_|| | \n" ^ " |       |       | \n" ^ " *****************" )

	end
