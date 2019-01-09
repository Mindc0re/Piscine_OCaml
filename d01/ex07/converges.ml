let rec converges f x n =
	if n < 0 then false
	else if x = (f x) then true
	else if converges (f) (f x) (n - 1) then true
	else false

let () =
	if (converges (( * ) 2) 2 5) then print_endline "True" else print_endline "False" ;
	if (converges (fun x -> x / 2) 2 3) then print_endline "True" else print_endline "False" ;
	if (converges (fun x -> x / 2) 2 2) then print_endline "True" else print_endline "False" ;
	if (converges (fun x -> x / 2) 14 2) then print_endline "True" else print_endline "False" ;
	if (converges (fun x -> x ^ "a") "a" 2) then print_endline "True" else print_endline "False" ;
