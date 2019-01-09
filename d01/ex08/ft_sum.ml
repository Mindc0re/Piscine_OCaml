let ft_sum f a b =
	let rec loop f a b acc =
		if a = b then (f a) +. acc 
		else if b < a then nan
		else loop (f) (a + 1) b (acc +. (f a))
	in
	loop f a b 0.0

let () = 
	print_string "Testing sum => (i * i) with i = 1 to 10 : " ;
	print_float ( ft_sum (fun i -> float_of_int (i * i)) 1 10 ) ;

	print_string "\nTesting sum => i with i = 1 to 4 : " ;
	print_float ( ft_sum (fun i -> float_of_int i) 1 4 ) ;

	print_string "\nTesting sum => ((2 * i) + 1) with i = 1 to 4 : " ;
	print_float ( ft_sum (fun i -> float_of_int ((2 * i) + 1)) 1 4 ) ;

	print_string "\nTesting with upper_bound lower than lower_bound : " ;
	print_float ( ft_sum (fun i -> float_of_int i) 2 1 ) ; print_char '\n'