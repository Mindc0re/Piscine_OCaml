type board = Grid.g list 

let printBoard b =
	let rec printBoardAux lst n = 
	if n < 3 then match lst with
	| [] -> ()
	| g1::g2::g3::tl 	-> 	print_endline 	( 
							(Grid.toStringLine n g1) ^ " | " ^ 
							(Grid.toStringLine n g2) ^ " | " ^ 
							(Grid.toStringLine n g3)
											) ;
						printBoardAux lst (n + 1)
	| _::tl 			-> ()
	else match lst with
	| g1::g2::g3::tl when tl <> [] 	-> 	print_endline "---------------------" ;
										printBoardAux tl 0
	| _::tl 						-> ()
	| [] 							-> ()
	in
	printBoardAux b 0


let gState (state, _) = state 

let rec isWonLine g = match g with
| g1::g2::g3::tl when 	(gState g1) = (gState g2) &&
						(gState g2) = (gState g3) && 
						(gState g1) <> Grid.Cell.None && 
						(gState g2) <> Grid.Cell.None 
						-> true
| _::tl 				-> isWonLine tl
| [] 					-> false 

let isWonCrossCol g x y z = 
	let xState = gState (List.nth g x) in
	let yState = gState (List.nth g y) in
	let zState = gState (List.nth g z) in
	
	if	xState = yState && 
		yState = zState && 
		xState <> Grid.Cell.None &&
		yState <> Grid.Cell.None
		then true
	else false 

let rec isFull = function
    | []                                         -> true
    | (state, _)::tl when state = Grid.Cell.None -> false
    | _::tl                                      -> isFull tl

let isWon b = 
	if 	isFull b || isWonLine b ||
		isWonCrossCol b 0 3 6 || 
		isWonCrossCol b 1 4 7 ||
		isWonCrossCol b 2 5 8 ||
		isWonCrossCol b 0 4 8 ||
		isWonCrossCol b 2 4 6
	then true
	else false


let createBoard () =
	let rec createAux n acc =
		if n < 9 then createAux (n + 1) ( [(Grid.createGrid ())] @ acc )
		else acc
	in
	createAux 0 []

let isMoveValid gridN cell board =
	let grid = List.nth board gridN in
    match grid with
    | (state, _) when state <> Grid.Cell.None -> false
    | (state, _) when state = Grid.Cell.None  -> Grid.isCellEmpty cell grid
    | (_,_)                                   -> false

let updateBoard gridN cell player board =
	List.mapi ( fun n grid -> if n = gridN then (Grid.updateCell gridN cell player grid) else grid ) board
