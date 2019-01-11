type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

let toInt value = match value with
	| T2	-> 1
	| T3  	-> 2
	| T4  	-> 3
	| T5  	-> 4
	| T6  	-> 5
	| T7  	-> 6
	| T8  	-> 7
	| T9  	-> 8
	| T10 	-> 9
	| Jack 	-> 10
	| Queen -> 11
	| King  -> 12
	| As 	-> 13

let toString value = match value with
	| T2	-> "2"
	| T3  	-> "3"
	| T4  	-> "4"
	| T5  	-> "5"
	| T6  	-> "6"
	| T7  	-> "7"
	| T8  	-> "8"
	| T9  	-> "9"
	| T10 	-> "10"
	| Jack 	-> "J"
	| Queen -> "Q"
	| King  -> "K"
	| As 	-> "A"

let toStringVerbose value = match value with
	| T2	-> "2"
	| T3  	-> "3"
	| T4  	-> "4"
	| T5  	-> "5"
	| T6  	-> "6"
	| T7  	-> "7"
	| T8  	-> "8"
	| T9  	-> "9"
	| T10 	-> "10"
	| Jack 	-> "Jack"
	| Queen -> "Queen"
	| King  -> "King"
	| As 	-> "As"

let next value =
	let rec loop l = match l with
		| hd::nx::tl when hd = value -> nx
		| hd::tl -> loop tl
		| [] -> invalid_arg "No value behind As"
	in
	loop all

let previous value =
	let rec loop l = match l with
		| hd::nx::tl when nx = value -> hd
		| hd::tl -> loop tl
		| [] -> invalid_arg "No value below T2"
	in
	loop all