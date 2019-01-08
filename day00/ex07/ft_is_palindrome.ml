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
	if ft_is_palindrome "hello" then print_endline "OK" else print_endline "PAS OK";
	if ft_is_palindrome "madam" then print_endline "OK" else print_endline "PAS OK";
	if ft_is_palindrome "fbuiewbfiw" then print_endline "OK" else print_endline "PAS OK";
	if ft_is_palindrome "radar" then print_endline "OK" else print_endline "PAS OK";
	if ft_is_palindrome "" then print_endline "OK" else print_endline "PAS OK";
	if ft_is_palindrome "f" then print_endline "OK" else print_endline "PAS OK"

let () = main ()