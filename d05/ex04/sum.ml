let sum a b = a +. b

let () =
	print_string "1. + 2. = " ; print_float (sum 1. 2.) ; print_newline () ;
	print_string "0. + 2. = " ; print_float (sum 0. 2.) ; print_newline () ;
	print_string "-1. + 1. = " ; print_float (sum (-1.) 1.) ; print_newline () ;
	print_string "1232323124141. + 1232323124141. = " ; print_float (sum 1232323124141. 1232323124141.) ; print_newline () ;
	print_string "1.75 + 2.36 = " ; print_float (sum 1.75 2.36) ; print_newline () ;
	print_string "1.121212 + 2.232323 = " ; print_float (sum 1.121212 2.232323) ; print_newline () ;
	print_string "1.000001 + 2.000000001 = " ; print_float (sum 1.000001 2.000000001) ; print_newline ()