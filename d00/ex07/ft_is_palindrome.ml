let ft_is_palindrome str = 
	let len = String.length str in
	if len = 0 || len = 1 then true
	else
	let i = 0 in
	let rec loop i = 
		if i > len / 2 then true
		else
		begin
			if String.get str i = String.get str (len - i - 1) then loop (i + 1)
			else false
		end
	in
	loop i
	

let main () =
	print_string "Testing \"the cake is a lie\" : " ; 
	if ft_is_palindrome "the cake is a lie" then print_endline "true" else print_endline "false";

	print_string "Testing \"madam\" : " ;
	if ft_is_palindrome "madam" then print_endline "true" else print_endline "false";

	print_string "Testing \"lalala alalal\" : " ;
	if ft_is_palindrome "lalala alalal" then print_endline "true" else print_endline "false";

	print_string "Testing \"radar\" : " ;
	if ft_is_palindrome "radar" then print_endline "true" else print_endline "false";

	print_string "Testing \"\" : " ;
	if ft_is_palindrome "" then print_endline "true" else print_endline "false";

	print_string "Testing \"f\" : " ;
	if ft_is_palindrome "f" then print_endline "true" else print_endline "false" ;

	print_string "Testing \"ffff\" : " ;
	if ft_is_palindrome "ffff" then print_endline "true" else print_endline "false"

let () = main ()