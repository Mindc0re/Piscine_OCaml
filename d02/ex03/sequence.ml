let add_tuple (x, y) liste = [x] @ ([y] @ liste)

let make_list_with_tuple tup =
	if tup = [] then [1]
	else
	let rec loop tup acc = match tup with
		| head::tail -> loop tail (add_tuple head acc)
		| [] -> acc
	in loop tup []

let rec make_str_with_list liste =
	if liste = [] then ""
	else 
	let rec loop liste acc = match liste with
		| head::tail -> loop tail ( acc ^ (string_of_int head) )
		| [] -> acc
	in
	loop liste ""

let sequence n =
	if n <= 0 then ""
	else
	let rec loop n prev_list acc_tup acc_n =
		if n <= 1 then make_str_with_list (prev_list)
		else match prev_list with
		| head::next::tail when head = next -> loop n (next :: tail) acc_tup (acc_n + 1)
		| head::tail -> loop n tail ([(acc_n, head)] @ acc_tup) 1
		| [] -> loop (n - 1) (make_list_with_tuple acc_tup) [] 1
	in
	loop n [1] [] 1


let () =
	print_endline "Testing the first 7 entries of the sequence :" ;
	print_endline (sequence 1) ;
	print_endline (sequence 2) ;
	print_endline (sequence 3) ;
	print_endline (sequence 4) ;
	print_endline (sequence 5) ;
	print_endline (sequence 6) ;
	print_endline (sequence 7) ;
	print_string "\nTesting with 0 : " ; print_endline (sequence 0) ;
	print_string "\nTesting with -1 : " ; print_endline (sequence (-1))