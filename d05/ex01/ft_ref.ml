type 'a ft_ref = {mutable content : 'a}

let return a = {content = a}

let get r = r.content

let set r a = r.content <- a

let bind r (f : 'a -> 'b ft_ref) = f r.content

let () =
	let aRef = return 42 in
	print_int (get aRef) ; print_newline () ;
	set aRef 21 ; print_int (get aRef) ; print_newline () ;
	let bRef = ( bind aRef (fun a -> return (float_of_int a)) ) in
	print_float (get bRef) ; print_newline ()