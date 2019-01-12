let rec print_list l = match l with
	| hd::tl -> print_endline hd ; print_list tl
	| [] -> print_newline ()

let drawAll deck = 
	let rec drawAllAux ( (card : Deck.Card.t) , (deckAux : Deck.t) ) =
		print_endline ( (Deck.Card.toStringVerbose card) ^ ", aka " ^ (Deck.Card.toString card) ^ " drawn" ) ;
		drawAllAux (Deck.drawCard deckAux)
	in drawAllAux (Deck.drawCard deck)

let () = 
	let deck = Deck.newDeck () in
	let dStr = Deck.toStringList deck in
	let dVerb = Deck.toStringListVerbose deck in
	print_list dStr ; print_list dVerb ; print_newline () ;
	drawAll deck