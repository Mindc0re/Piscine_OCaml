let rec reflect_gray_0 seq = match seq with
	| tete::suite::queue -> ("0" ^ tete ^ " ") ^ (reflect_gray_0 (suite::queue))
	| tete::queue -> "0" ^ tete
	| [] -> ""

let rec reflect_gray_1 seq = match seq with
	| tete::suite::queue -> (reflect_gray_1 (suite::queue)) ^ (" 1" ^ tete)
	| tete::queue -> "1" ^ tete
	| [] -> ""

let gray n =
	if n = 0 then ""
	else
	let rec gray_loop n prev_str =
		let prev_str_list = (String.split_on_char ' ' prev_str) in
			if n = 1 then prev_str
			else gray_loop (n - 1) ((reflect_gray_0 prev_str_list) ^ " " ^ (reflect_gray_1 prev_str_list))
	in
	gray_loop n "0 1"

let gray_1 = (String.split_on_char ' ' "0 1")

let () =
	print_string (gray 1)