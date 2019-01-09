let crossover listA listB =
	if listA = [] || listB = [] then []
	else
	let rec loopA listA listB list_cross =
		match listA with
		| head::queue when (loopB head listB) -> loopA queue listB ( list_cross @ (head :: []) )
		| head::queue -> loopA queue listB list_cross
		| [] -> list_cross
	and loopB elementA listB =
		match listB with
		| head::queue when head = elementA -> true
		| head::queue -> loopB elementA queue
		| [] -> false
	in
	loopA listA listB []

(* Test print function *)
let rec print_list liste func = match liste with
	| [] -> print_char '\n'
	| head::queue -> (func head) ; print_char ' ' ; print_list queue (func)

(* Test lists *)
let empty = []

let intA = [1; 2; 3;]
let intB = [4; 5; 3; 1; 3;]
let intC = [8; 9; 10]

let charA = ['a'; 'b'; 'c']
let charB = ['b'; 'z'; 'w']
let charC = ['i']

let () =
	print_string "Two int lists and 2 corresponding entries : " ; print_list (crossover intA intB) (print_int) ;
	print_string "Two int lists and no corresponding entries : " ; print_list (crossover intA intC) (print_int) ;
	print_string "An int list and an empty list : " ; print_list (crossover intA empty) (print_int) ;
	print_string "An empty list and an int list : " ; print_list (crossover empty intA) (print_int) ;

	print_string "Two char lists and 1 corresponding entry : " ; print_list (crossover charA charB) (print_char) ;
	print_string "Two char lists and no corresponding entries : " ; print_list (crossover charA charC) (print_char)
