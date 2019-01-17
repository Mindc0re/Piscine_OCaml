let () =
	let ppl1 = new people ("Francis") in
	print_endline ppl1#tostring ;
	ppl1#talk ;
	ppl1#die ;