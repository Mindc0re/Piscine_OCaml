let () =
	let met = new Alkanes.met in
	let eth = new Alkanes.eth in
	let pro = new Alkanes.pro in
	let but = new Alkanes.but in
	let pen = new Alkanes.pen in
	let hex = new Alkanes.hex in
	let hep = new Alkanes.hep in
	let oct = new Alkanes.oct in
	let non = new Alkanes.non in
	let dec = new Alkanes.dec in
	let und = new Alkanes.und in
	let dod = new Alkanes.dod in

	print_endline met#to_string ;
	print_endline eth#to_string ;
	print_endline pro#to_string ;
	print_endline but#to_string ;
	print_endline pen#to_string ;
	print_endline hex#to_string ;
	print_endline hep#to_string ;
	print_endline oct#to_string ;
	print_endline non#to_string ;
	print_endline dec#to_string ;
	print_endline und#to_string ;
	print_endline dod#to_string	;

	print_endline ( "methane = propane is " ^ (if met#equals pro then "true" else "false") ) ;
	print_endline ( "methane = methane is " ^ (if met#equals met then "true" else "false") )