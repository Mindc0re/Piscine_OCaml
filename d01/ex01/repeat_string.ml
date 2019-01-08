let repeat_string ?(str="x") n =
	if n < 0 then "Error"
	else
	let rec loop n acc =
		if n = 0 then acc
		else loop (n - 1) (acc ^ str)
	in
	loop n ""

let () =
	print_string "Testing default with -42 : " ; print_endline ( repeat_string (-42) ) ;
	print_string "Testing default with 0 : " ; print_endline ( repeat_string 0 ) ;
	print_string "Testing default with 1 : " ; print_endline ( repeat_string 1 ) ;
	print_string "Testing default with 5 : " ; print_endline ( repeat_string 5 ) ;
	print_string "Testing \"toto\" -42 times : " ; print_endline ( repeat_string ~str:"toto" (-42) ) ;
	print_string "Testing \"toto\" 0 times : " ; print_endline ( repeat_string ~str:"toto" 0 ) ;
	print_string "Testing \"toto\" 1 times : " ; print_endline ( repeat_string ~str:"toto" 1 ) ;
	print_string "Testing \"What ?\" 4 times : " ; print_endline ( repeat_string ~str:"What ?" 4 ) ;
	print_string "Testing \"42\" 10 times : " ; print_endline ( repeat_string ~str:"42" 10 )