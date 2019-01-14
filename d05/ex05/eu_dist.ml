let eu_fun a b index = (a.(index) -. b.(index)) *. (a.(index) -. b.(index))

let eu_dist a b =
	if Array.length a = 0 || Array.length b = 0 then invalid_arg "Empty array provided" ;
	let rec eu_dist_aux max acc =
		if max = 1 then ( eu_fun a b (max - 1) ) +. acc
		else eu_dist_aux (max - 1) ( acc +. eu_fun a b (max - 1) )
	in
	sqrt ( eu_dist_aux (Array.length a) 0.0 )

let () =
	let aArr = [| (-1.) ; 2. ; 3. |] in
	let bArr = [| 4. ; 0. ; (-3.) |] in
	let empty = [| |] in
	try 
		print_float (eu_dist aArr bArr) ; print_newline () ;
		print_float (eu_dist aArr empty) ; print_newline ()
	with Invalid_argument err -> print_endline err
