let fibonacci n =
	let rec loop n x y =
		if n < 0 then (-1)
		else if n = 0 then x
		else loop (n - 1) y (x + y)
	in
	loop n 0 1

let () =
	print_endline "Testing with a negative number" ;
	print_int ( fibonacci (-66) ) ; print_string "\n\n" ;
	print_endline "Testing with the 10 first numbers of the Fibonacci sequence" ;
	print_int (fibonacci 0) ; print_char '\n' ;
	print_int (fibonacci 1) ; print_char '\n' ;
	print_int (fibonacci 2) ; print_char '\n' ;
	print_int (fibonacci 3) ; print_char '\n' ;
	print_int (fibonacci 4) ; print_char '\n' ;
	print_int (fibonacci 5) ; print_char '\n' ;
	print_int (fibonacci 6) ; print_char '\n' ;
	print_int (fibonacci 7) ; print_char '\n' ;
	print_int (fibonacci 8) ; print_char '\n' ;
	print_int (fibonacci 9) ; print_char '\n' ;
	print_int (fibonacci 10) ; print_char '\n'
