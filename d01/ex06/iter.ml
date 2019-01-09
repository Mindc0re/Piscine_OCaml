let rec iter f x n =
	if n < 0 then (-1)
	else if n = 0 then x
	else iter (f) (f x) (n - 1)

let () =
	print_string "Testing 2 and 4 with f = x * x : " ;
	print_int (iter (fun x -> x * x) 2 4) ;

	print_string "\nTesting 2 and 4 with f = x * 2 : " ;
	print_int (iter (fun x -> x * 2) 2 4) ;

	print_string "\nTesting 10 and 3 with f = x + 42 : " ;
	print_int (iter (fun x -> x + 42) 10 3) ;

	print_string "\nTesting 42 and 84 with f = 0 : " ;
	print_int (iter (fun x -> 0) 42 84) ;