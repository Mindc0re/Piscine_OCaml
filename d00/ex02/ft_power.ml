let rec ft_power n pow =
	if n = 0 then 0
	else if pow = 0 then 1
	else n * (ft_power n (pow - 1))

let main () =
	print_string "Testing 1 to the power of 1 (1^1) : " ; print_int (ft_power 1 1) ; print_char '\n' ;
	print_string "Testing 42^0 : " ; print_int (ft_power 42 0) ; print_char '\n' ;
	print_string "Testing 0^42 : " ; print_int (ft_power 0 42) ; print_char '\n' ;
	print_string "Testing 4^2 : " ; print_int (ft_power 4 2) ; print_char '\n'

let () = main ()