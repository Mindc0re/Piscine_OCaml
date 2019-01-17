let () =
	let idiotNumberOne 	= new People.people ("Goofy") in
	let doc 			= new Doctor.doctor ("Doctor") in
	let goodDalek 		= new Dalek.dalek in

	print_newline () ;

	print_endline goodDalek#to_string ;

	goodDalek#talk ;
	goodDalek#talk ;
	goodDalek#talk ;
	goodDalek#exterminate (idiotNumberOne) ;

	print_endline goodDalek#to_string ;

	doc#use_sonic_screwdriver ;
	goodDalek#die ;
	print_endline goodDalek#to_string