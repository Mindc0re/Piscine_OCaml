module MyStringHashTable =
struct
	type t = string

	let equal t1 t2 = (t1 = t2)

	let hash t1 = 
		let len = String.length t1 in
		let rec hashAux i acc =
		if i = len then acc
		else hashAux (i + 1) ( acc + (int_of_char (String.get t1 i) * 5) )
		in
		hashAux 0 0
end

module StringHashtbl : (Hashtbl.S with type key := MyStringHashTable.t) = Hashtbl.Make (MyStringHashTable)

let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
	List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
	StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht