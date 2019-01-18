let () =
	let h2o = new Molecules.water in
	let co2 = new Molecules.carbonDioxyde in
	let tnt = new Molecules.tnt in
	let chl = new Molecules.chl in
	let pen = new Molecules.pen in

	print_endline h2o#to_string ;
	print_endline co2#to_string ;
	print_endline tnt#to_string ;
	print_endline chl#to_string ;
	print_endline pen#to_string ;

	print_endline ( "h2o = co2 is " ^ (if h2o#equals co2 then "true" else "false") ) ;
	print_endline ( "h2o = h2o is " ^ (if h2o#equals h2o then "true" else "false") )