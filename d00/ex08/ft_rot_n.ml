let lower_char c = 
	if c >= 'a' && c <= 'z' then 'a' else 'A'

let upper_char c =
	if c >= 'a' && c <= 'z'	then 'z' else 'Z'

let is_not_alpha c =
	if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')	then false else true

let ft_rot_n n str =
	let true_n = n mod 26 in
	let rot_char c = 
		if (true_n = 0) || (is_not_alpha c) then c
		else if (int_of_char c + true_n) > (int_of_char (upper_char c)) 
		then char_of_int ( (int_of_char (lower_char c)) + (((int_of_char c) + n) mod (int_of_char (upper_char c))) - 1 )
		else char_of_int ( (int_of_char c) + true_n )
	in
	String.map rot_char str


let main () =
	print_string "Testing 1 rotation of \"abcdefghijklmnopqrstuvwxyz\" : " ;
	print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz") ;

	print_string "Testing 13 rotation of \"abcdefghijklmnopqrstuvwxyz\" : " ;
	print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz") ;

	print_string "Testing 0 rotation of \"Damned !\" : " ;
	print_endline (ft_rot_n 0 "Damned !") ;

	print_string "Testing 42 rotation of \"\" : " ;
	print_endline (ft_rot_n 42 "") ;

	print_string "Testing 2 rotation of \"0123456789\" : " ;
	print_endline (ft_rot_n 2 "0123456789") ;

	print_string "Testing 2 rotation of \"OI2EAS67B9\" : " ;
	print_endline (ft_rot_n 2 "OI2EAS67B9") ;

	print_string "Testing 1 rotation of \"NBzlk qnbjr !\" : " ;
	print_endline (ft_rot_n 1 "NBzlk qnbjr !")


let () = main ()