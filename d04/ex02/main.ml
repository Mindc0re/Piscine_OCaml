let rec print_str_card cardList = match cardList with
	| hd::tl -> print_string (Card.toString hd) ; print_char ' ' ; print_str_card tl
	| [] -> print_char '\n'

let rec print_verb_card cardList = match cardList with
	| hd::tl -> print_endline (Card.toStringVerbose hd) ; print_verb_card tl
	| [] -> print_char '\n'


let () = 
	print_endline "Printing simple string form of all cards " ;
	print_str_card Card.all ;

	let fiveSpade = Card.newCard Card.Value.T5 Card.Color.Spade in
	print_string "\nCreating new card : " ; 
	print_string (Card.toString fiveSpade) ; 
	print_string " ==> " ; 
	print_endline (Card.toStringVerbose fiveSpade) ;

	let nineHeart = Card.newCard Card.Value.T9 Card.Color.Heart in
	print_string "Creating new card : " ; 
	print_string (Card.toString nineHeart) ; 
	print_string " ==> " ; 
	print_endline (Card.toStringVerbose nineHeart) ;

	print_string "\nComparing 5 of Spades with 9 of Hearts => " ; print_int (Card.compare fiveSpade nineHeart) ; print_char '\n' ;
	print_string "Comparing 9 of Hearts 5 of Spades      => " ; print_int (Card.compare nineHeart fiveSpade) ; print_char '\n' ;
	print_string "Comparing one card with itself         => " ; print_int (Card.compare fiveSpade fiveSpade) ; print_char '\n' ;

	print_string "\nMax of 5S and 9H is => " ; print_endline (Card.toString (Card.max fiveSpade nineHeart)) ;
	print_string "Max of 5S and 5S is => " ; print_endline (Card.toString (Card.max fiveSpade fiveSpade)) ;
	print_string "Min of 5S and 9H is => " ; print_endline (Card.toString (Card.min fiveSpade nineHeart)) ;
	print_string "Min of 5S and 5S is => " ; print_endline (Card.toString (Card.min fiveSpade fiveSpade)) ;

	print_string "\nSelecting best card of Clubs                => " ; 
	print_endline (Card.toStringVerbose (Card.best Card.allClubs)) ;
	
	let singleList = [fiveSpade] in
	print_string "Selecting best card of single-element list  => " ; 
	print_endline (Card.toStringVerbose (Card.best singleList)) ;

	let sameList = [nineHeart; nineHeart; nineHeart] in
	print_string "Selecting best card of \"same-elements\" list => " ; 
	print_endline (Card.toStringVerbose (Card.best sameList)) ;

	print_string "\nTesting if \"Five of Spades\" \"isOf Spade\" => " ;
	print_endline (if (Card.isOf fiveSpade Card.Color.Spade) then "true" else "false") ;
	print_string "Testing if \"Five of Spades\" \"isOf Club\"  => " ;
	print_endline (if (Card.isOf fiveSpade Card.Color.Club) then "true" else "false") ;
	print_string "Testing if \"Five of Spades\" \"isSpade\"    => " ;
	print_endline (if (Card.isSpade fiveSpade) then "true" else "false") ;
	print_string "Testing if \"Five of Spades\" \"isHeart\"    => " ;
	print_endline (if (Card.isHeart fiveSpade) then "true" else "false") ;
	print_string "Testing if \"Five of Spades\" \"isClub\"     => " ;
	print_endline (if (Card.isClub fiveSpade) then "true" else "false") ;
	print_string "Testing if \"Five of Spades\" \"isDiamond\"  => " ;
	print_endline (if (Card.isDiamond fiveSpade) then "true" else "false") ;