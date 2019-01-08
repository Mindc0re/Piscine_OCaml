let repeat_x n =
	if n < 0 then "Error"
	else
	let rec loop n acc =
		if n = 0 then acc
		else loop (n - 1) (acc ^ "x")
	in
	loop n ""

let () =
	print_string "Testing with -42 : " ; print_endline ( repeat_x (-42) ) ;
	print_string "Testing with 0 : " ; print_endline ( repeat_x 0 ) ;
	print_string "Testing with 1 : " ; print_endline ( repeat_x 1 ) ;
	print_string "Testing with 5 : " ; print_endline ( repeat_x 5 ) ;
	print_string "Testing with 42 : " ; print_endline ( repeat_x 42 )