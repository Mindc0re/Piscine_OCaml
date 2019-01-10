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


(* Test *)

let nucl_base_str base = match base with
	| A -> "A"
	| T -> "T"
	| C -> "C"
	| G -> "G"
	| None -> ""

let () =
	let nuclA = generate_nucleotide 'A' in
	let nuclT = generate_nucleotide 'T' in
	let nuclC = generate_nucleotide 'C' in
	let nuclG = generate_nucleotide 'G' in
	let nuclNone = generate_nucleotide 'W' in
	print_endline ( nuclA.phos ^ ", " ^ nuclA.deox ^ ", " ^ (nucl_base_str nuclA.base) ) ;
	print_endline ( nuclT.phos ^ ", " ^ nuclT.deox ^ ", " ^ (nucl_base_str nuclT.base) ) ;
	print_endline ( nuclC.phos ^ ", " ^ nuclC.deox ^ ", " ^ (nucl_base_str nuclC.base) ) ;
	print_endline ( nuclG.phos ^ ", " ^ nuclG.deox ^ ", " ^ (nucl_base_str nuclG.base) ) ;
	print_endline ( nuclNone.phos ^ ", " ^ nuclNone.deox ^ ", " ^ (nucl_base_str nuclNone.base) ) 