let ft_print_alphabet () =
	let a_char = int_of_char 'a' in
	let z_char = int_of_char 'z' in
	let rec loop_char curr_char =
		if curr_char <= z_char 
		then let curr_print = char_of_int curr_char in
		print_char curr_print ; 
		loop_char (curr_char + 1)
	in
	loop_char a_char;
	print_char '\n'

let main () =
	ft_print_alphabet ()

let () = main ()