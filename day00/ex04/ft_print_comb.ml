let ft_print_comb () =
	let a = 0 in
	let b = 1 in
	let c = 2 in
	let rec loop_truc a b c =
		if c > 9 then loop_truc a (b + 1) 2
		else if b > 8 then loop_truc (a + 1) 2 3
		else if a > 7 then print_string "\n"
		else if a <= 7 && b <= 8 && c <= 9 && a <> b && b <> c && a <> c && a < b && b < c then 
		begin 
			print_int a ; 
			print_int b ; 
			print_int c ; 
			if a <= 9 && b <= 8 && a <> 7
			then print_string ", " ;
			loop_truc a b (c + 1) 
		end
		else if c <= 9 then loop_truc a b (c + 1)
	in
	loop_truc a b c

let main () =
	ft_print_comb ()

let () = main ()