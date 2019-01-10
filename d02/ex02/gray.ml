let rec reflect_gray_0 seq = match seq with
	| head::next::tail -> ("0" ^ head ^ " ") ^ (reflect_gray_0 (next::tail))
	| head::tail -> "0" ^ head
	| [] -> ""

let rec reflect_gray_1 seq = match seq with
	| head::next::tail -> (reflect_gray_1 (next::tail)) ^ (" 1" ^ head)
	| head::tail -> "1" ^ head
	| [] -> ""

let gray n =
	if n <= 0 then ""
	else
	let rec gray_loop n prev_str =
		let prev_str_list = (String.split_on_char ' ' prev_str) in
			if n = 1 then prev_str ^ "\n"
			else gray_loop (n - 1) ((reflect_gray_0 prev_str_list) ^ " " ^ (reflect_gray_1 prev_str_list))
	in
	gray_loop n "0 1"

let () =
	print_endline "Testing the 5 first sequences of Gray :" ;
	print_string (gray 1) ;
	print_string (gray 2) ;
	print_string (gray 3) ; 
	print_string (gray 4) ;
	print_string (gray 5) ;
	
	print_endline "\nTesting with gray 0 :" ;
	print_string (gray 0) ;

	print_endline "\nTesting with gray -1 :" ;
	print_string (gray (-1))