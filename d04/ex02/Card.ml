module Color = 
struct
	type t = Spade | Heart | Diamond | Club

	let all = [Spade; Heart; Diamond; Club]

	let toString card = match card with
		| Spade -> "S"
		| Heart -> "H"
		| Diamond -> "D"
		| Club -> "C"

	let toStringVerbose card = match card with
		| Spade -> "Spade"
		| Heart -> "Heart"
		| Diamond -> "Diamond"
		| Club -> "Club"
end

module Value = 
struct
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
end

(* Type t of Card *)
type t = 
{
	value : Value.t ;
	color : Color.t
}


(* New card function *)
let newCard v c = { value = v ; color = c }


(* Card lists *)
let allSpades = [
					{ value = Value.T2 ; 	color = Color.Spade } ;
					{ value = Value.T3 ; 	color = Color.Spade } ;
					{ value = Value.T4 ; 	color = Color.Spade } ;
					{ value = Value.T5 ; 	color = Color.Spade } ;
					{ value = Value.T6 ; 	color = Color.Spade } ;
					{ value = Value.T7 ; 	color = Color.Spade } ;
					{ value = Value.T8 ; 	color = Color.Spade } ;
					{ value = Value.T9 ; 	color = Color.Spade } ;
					{ value = Value.T10 ; 	color = Color.Spade } ;
					{ value = Value.Jack ; 	color = Color.Spade } ;
					{ value = Value.Queen ; color = Color.Spade } ;
					{ value = Value.King ; 	color = Color.Spade } ;
					{ value = Value.As ; 	color = Color.Spade }
				]

let allHearts = [
					{ value = Value.T2 ; 	color = Color.Heart } ;
					{ value = Value.T3 ; 	color = Color.Heart } ;
					{ value = Value.T4 ; 	color = Color.Heart } ;
					{ value = Value.T5 ; 	color = Color.Heart } ;
					{ value = Value.T6 ; 	color = Color.Heart } ;
					{ value = Value.T7 ; 	color = Color.Heart } ;
					{ value = Value.T8 ; 	color = Color.Heart } ;
					{ value = Value.T9 ; 	color = Color.Heart } ;
					{ value = Value.T10 ; 	color = Color.Heart } ;
					{ value = Value.Jack ; 	color = Color.Heart } ;
					{ value = Value.Queen ; color = Color.Heart } ;
					{ value = Value.King ; 	color = Color.Heart } ;
					{ value = Value.As ; 	color = Color.Heart }
				]

let allDiamonds = [
					{ value = Value.T2 ; 	color = Color.Diamond } ;
					{ value = Value.T3 ; 	color = Color.Diamond } ;
					{ value = Value.T4 ; 	color = Color.Diamond } ;
					{ value = Value.T5 ; 	color = Color.Diamond } ;
					{ value = Value.T6 ; 	color = Color.Diamond } ;
					{ value = Value.T7 ; 	color = Color.Diamond } ;
					{ value = Value.T8 ; 	color = Color.Diamond } ;
					{ value = Value.T9 ; 	color = Color.Diamond } ;
					{ value = Value.T10 ; 	color = Color.Diamond } ;
					{ value = Value.Jack ; 	color = Color.Diamond } ;
					{ value = Value.Queen ; color = Color.Diamond } ;
					{ value = Value.King ; 	color = Color.Diamond } ;
					{ value = Value.As ; 	color = Color.Diamond }
				]

let allClubs = [
					{ value = Value.T2 ; 	color = Color.Club } ;
					{ value = Value.T3 ; 	color = Color.Club } ;
					{ value = Value.T4 ; 	color = Color.Club } ;
					{ value = Value.T5 ; 	color = Color.Club } ;
					{ value = Value.T6 ; 	color = Color.Club } ;
					{ value = Value.T7 ; 	color = Color.Club } ;
					{ value = Value.T8 ; 	color = Color.Club } ;
					{ value = Value.T9 ; 	color = Color.Club } ;
					{ value = Value.T10 ; 	color = Color.Club } ;
					{ value = Value.Jack ; 	color = Color.Club } ;
					{ value = Value.Queen ; color = Color.Club } ;
					{ value = Value.King ; 	color = Color.Club } ;
					{ value = Value.As ; 	color = Color.Club }
				]

let all = List.append allSpades (List.append allHearts (List.append allDiamonds allClubs))


(* Getters *)
let getValue card = card.value

let getColor card = card.color


(* ToString functions *)
let toString card =
	Printf.sprintf ("%s%s") (Value.toString card.value) (Color.toString card.color)

let toStringVerbose card =
	Printf.sprintf ("Card(%s, %s)") (Value.toStringVerbose card.value) (Color.toStringVerbose card.color)


(* Two-card comparison functions *)
let compare cardA cardB = 
	if cardA = cardB then 0
	else if cardA < cardB then (-1)
	else 1

let min cardA cardB = 
	if cardA = cardB then cardA
	else if cardA < cardB then cardA
	else cardB

let max cardA cardB = 
	if cardA = cardB then cardA
	else if cardA > cardB then cardA
	else cardB

let best cardList =
	if cardList = [] then invalid_arg "Card list provided is empty"
	else
	List.fold_left (max) (List.hd cardList) cardList


(* One-card comparison functions *)
let isOf card color = (card.color = color)

let isSpade card = isOf card Spade

let isHeart card = isOf card Heart

let isDiamond card = isOf card Diamond

let isClub card = isOf card Club
