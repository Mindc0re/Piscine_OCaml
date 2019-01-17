class dalek =
	object

		val _name:string 			= "Dalek" ^ String.init 3 (fun i -> Random.self_init () ; match Random.int 2 with | 0 -> char_of_int( (Random.int 27) + 65 ) | _ -> char_of_int( (Random.int 27) + 97 ) )
		val mutable _hp:int			= 100
		val mutable _shield:bool 	= true

		method to_string 	= _name ^ ", HP : " ^ string_of_int _hp ^ ", shield : " ^ (if _shield = true then "true" else "false")
		method talk 		= if _hp > 0 then 
								begin Random.self_init () ; 
								match Random.int 4 with
								| 0 -> print_endline "Explain! Explain!"
								| 1 -> print_endline "Exterminate! Exterminate!"
								| 2 -> print_endline "I obey!"
								| _ -> print_endline "You are the Doctor! You are the enemy of the Daleks!"
								end

		method exterminate (ppl:People.people) = if _hp > 0 then begin _shield <- (if _shield = true then false else true) ; ppl#die end
		method die			= if _hp > 0 then begin _hp <- 0 ; print_endline "Emergency Temporal Shift!" end
	end
