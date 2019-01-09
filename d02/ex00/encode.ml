let encode liste = 
	if liste = [] then []
	else
	let rec loop liste acc_tup acc_n = match liste with
		| [] -> acc_tup
		| head::next::queue when head = next -> loop (next :: queue) acc_tup (acc_n + 1)
		| head::queue -> loop queue ( acc_tup @ [(acc_n, head)] ) 1
	in
	loop liste [] 1



(* Test print functions *)
let print_char_tuple (x , y) = print_int x ; print_char y
let print_int_tuple (x , y) = print_int x ; print_int y
let print_string_tuple (x , y) = print_int x ; print_string y

let rec print_list liste func = match liste with
	| [] -> print_char '\n'
	| head::queue -> (func head) ; print_list queue (func)

(* Test lists *)
let empty = []
let char_list = ['a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'c'; 'd']
let int_list = [1; 1; 1; 2; 2; 3; 4; 2]
let string_list = ["toto"; "zaz"; "zaz"; "toto"; "toto"; "toto"]

let () = 
	print_string "Testing with list of char : " ; print_list (encode char_list) (print_char_tuple) ;
	print_string "\nTesting with list of int : " ; print_list (encode int_list) (print_int_tuple) ;
	print_string "\nTesting with list of string : " ; print_list (encode string_list) (print_string_tuple) ;
	print_string "\nTesting with empty list : " ; print_list (encode empty) (print_string_tuple)
