let () =
	let h = new Atoms.hydrogen in
	let h2 = new Atoms.hydrogen in
	let c = new Atoms.carbon in
	let o = new Atoms.oxygen in
	let s = new Atoms.sodium in
	let t = new Atoms.titanium in
	let g = new Atoms.gold in

	print_endline h#to_string ;
	print_endline c#to_string ;
	print_endline o#to_string ;
	print_endline s#to_string ;
	print_endline t#to_string ;
	print_endline g#to_string ;

	print_endline ( "h = h2 is " ^ (if h#equals h2 then "true" else "false") ) ;
	print_endline ( "g = o is " ^ (if g#equals o then "true" else "false") )