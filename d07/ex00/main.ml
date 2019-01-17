let () =
	let ppl1 = new People.people ("Francis") in
	print_endline ppl1#to_string ;
	ppl1#talk ;
	ppl1#die ;