let ft_test_sign n =
	if n >= 0
		then print_endline "positive"
	else print_endline "negative"


let main () =
	print_string "Testing with 5 : " ; ft_test_sign 5 ;
	print_string "Testing with 0 : " ; ft_test_sign 0 ;
	print_string "Testing with -1 : " ; ft_test_sign (-1)

let () = main ()