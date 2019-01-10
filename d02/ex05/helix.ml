(* Nucleotide *)
type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = 
{
	phos : phosphate ;
	deox : deoxyribose ;
	base : nucleobase
}

let generate_nucleotide c = match c with
	| 'A' -> { phos = "phosphate" ; deox = "deoxyribose" ; base = A }
	| 'T' -> { phos = "phosphate" ; deox = "deoxyribose" ; base = T }
	| 'C' -> { phos = "phosphate" ; deox = "deoxyribose" ; base = C }
	| 'G' -> { phos = "phosphate" ; deox = "deoxyribose" ; base = G }
	|  _  -> { phos = "phosphate" ; deox = "deoxyribose" ; base = None }

(* Helix *)
type helix = nucleotide list

let generate_helix (n : int) : helix = 
	Random.self_init () ;
	let rec loop n acc =
		if n = 0 then acc
		else match Random.int 4 with
		| 0 -> loop (n - 1) ( [(generate_nucleotide 'A')] @ acc )
		| 1 -> loop (n - 1) ( [(generate_nucleotide 'T')] @ acc )
		| 2 -> loop (n - 1) ( [(generate_nucleotide 'C')] @ acc )
		| 3 -> loop (n - 1) ( [(generate_nucleotide 'G')] @ acc )
		| _ -> []
	in
	loop n []

let helix_to_string (h : helix) =
	let rec loop_list liste acc = match liste with
		| head::tail -> loop_list tail ( (string_of_base head.base) ^ acc)
		| [] -> acc
	and
	string_of_base base = match base with
		| A -> "A"
		| T -> "T"
		| C -> "C"
		| G -> "G"
		| None -> ""
	in
	loop_list h ""

let complementary_helix (h : helix) : helix =
	let rec loop h acc = match h with
		| head::tail -> loop tail ( acc @ [( generate_nucleotide (get_complementary head.base) )] )
		| [] -> acc
	and
	get_complementary base = match base with
		| A -> 'T'
		| T -> 'A'
		| C -> 'G'
		| G -> 'C'
		| None -> '0'
	in
	loop h []


(* Test *)

let () =
	let helixA = generate_helix 4 in
	let helixA_comp = complementary_helix helixA in
	print_endline (helix_to_string helixA) ;
	print_endline (helix_to_string helixA_comp)