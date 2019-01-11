let rec print_list l = match l with
	| hd::tl -> print_string (Color.toString hd) ; print_list tl
	| [] -> print_char '\n'

let () =
	let spade = Color.Spade in
	let heart = Color.Heart in
	let diamond = Color.Diamond in
	let club = Color.Club in
	
	print_endline (Color.toString spade) ;
	print_endline (Color.toStringVerbose spade) ;

	print_endline (Color.toString heart) ;
	print_endline (Color.toStringVerbose heart) ;

	print_endline (Color.toString diamond) ;
	print_endline (Color.toStringVerbose diamond) ;

	print_endline (Color.toString club) ;
	print_endline (Color.toStringVerbose club) ;

	print_list Color.all