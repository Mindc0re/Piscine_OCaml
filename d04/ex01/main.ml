let () = 
	let two = Value.T2 in
	let jack = Value.Jack in
	let ace = Value.As in

	print_int (Value.toInt two) ; print_string ", " ; 
	print_string (Value.toString two) ; print_string ", " ;
	print_endline (Value.toStringVerbose two) ;
	(* print_string "Previous is " ; print_endline (Value.toString (Value.previous two)) ; *)
	print_string "Next is " ; print_endline (Value.toStringVerbose (Value.next two)) ;

	print_char '\n' ; print_int (Value.toInt jack) ; print_string ", " ; 
	print_string (Value.toString jack) ; print_string ", " ;
	print_endline (Value.toStringVerbose jack) ;
	print_string "Previous is " ; print_endline (Value.toStringVerbose (Value.previous jack)) ;
	print_string "Next is " ; print_endline (Value.toStringVerbose (Value.next jack)) ;

	print_char '\n' ; print_int (Value.toInt ace) ; print_string ", " ; 
	print_string (Value.toString ace) ; print_string ", " ;
	print_endline (Value.toStringVerbose ace) ;
	print_string "Previous is " ; print_endline (Value.toStringVerbose (Value.previous ace)) ;
	(* print_string "Next is " ; print_endline (Value.toString (Value.next ace)) ; *)
