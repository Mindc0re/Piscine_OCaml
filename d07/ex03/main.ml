let print_army army =
	print_endline "\nArmy :" ;
	let rec print_army_aux a = match a with
		| hd::tl -> print_endline hd#to_string ; print_army_aux tl
		| [] -> print_newline ()
	in
	print_army_aux army

let () =
	let armyIdiots 	= ref new Army.army in
	let armyDocs	= ref new Army.army in
	let armyDaleks	= ref new Army.army in

	(* IDIOT ARMY *)
	print_endline "(***Idiots Army ***)" ;
	print_army !armyIdiots#get_army ;

	ignore(armyIdiots := (!armyIdiots#add (new People.people ("Goofy"))) ) ;
	print_army !armyIdiots#get_army ;

	ignore(armyIdiots := (!armyIdiots#add (new People.people ("Whatever"))) ) ;
	print_army !armyIdiots#get_army ;

	print_endline ("\nDeleting armyIdiots one by one") ;
	ignore(armyIdiots := !armyIdiots#delete) ;
	print_army !armyIdiots#get_army ;

	ignore(armyIdiots := !armyIdiots#delete) ;
	print_army !armyIdiots#get_army ;

	(* DOCTOR ARMY *)

	print_endline "(***Doctors Army ***)" ;
	print_army !armyDocs#get_army ;

	ignore(armyDocs := (!armyDocs#add (new Doctor.doctor ("Doctor"))) ) ;
	print_army !armyDocs#get_army ;

	print_endline ("\nDeleting armyDocs one by one") ;
	ignore(armyDocs := !armyDocs#delete) ;
	print_army !armyDocs#get_army ;


	(* DALEKS ARMY *)
	print_endline "(***Daleks Army ***)" ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := (!armyDaleks#add new Dalek.dalek) ) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := (!armyDaleks#add new Dalek.dalek) ) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := (!armyDaleks#add new Dalek.dalek) ) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := (!armyDaleks#add new Dalek.dalek) ) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := (!armyDaleks#add new Dalek.dalek) ) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := (!armyDaleks#add new Dalek.dalek) ) ;
	print_army !armyDaleks#get_army ;

	print_endline ("\nDeleting armyDaleks one by one") ;
	ignore(armyDaleks := !armyDaleks#delete) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := !armyDaleks#delete) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := !armyDaleks#delete) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := !armyDaleks#delete) ;
	print_army !armyDaleks#get_army ;

	ignore(armyDaleks := !armyDaleks#delete) ;
	print_army !armyDaleks#get_army ;