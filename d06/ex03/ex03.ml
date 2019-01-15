module type FRACTIONNAL_BITS =
sig
	val bits : int
end

module type FIXED = 
sig
	type t
	val of_float	: float -> t
	val of_int 		: int 	-> t
	val to_float 	: t 	-> float
	val to_int 		: t 	-> int
	val to_string 	: t 	-> string

	val zero 	: t
	val one 	: t
	val succ 	: t -> t
	val pred 	: t -> t
	val min 	: t -> t -> t
	val max 	: t -> t -> t
	val gth 	: t -> t -> bool
	val lth 	: t -> t -> bool
	val gte 	: t -> t -> bool
	val lte 	: t -> t -> bool
	val eqp 	: t -> t -> bool
	val eqs 	: t -> t -> bool
	val add 	: t -> t -> t
	val sub 	: t -> t -> t
	val mul 	: t -> t -> t
	val div 	: t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module type MAKE =
	functor (FractBits : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
	functor (FractBits : FRACTIONNAL_BITS) ->
	struct

		type t = { n : int }

		let roundf 		nVal	= 
			if mod_float ( nVal ) 1. = 0. then nVal
			else if mod_float ( nVal ) 1. >= 0.5 then ceil nVal
			else floor nVal

		let of_float 	nVal 	= 	{ n = int_of_float ( roundf (nVal *. float_of_int (1 lsl FractBits.bits)) ) }
		
		let of_int 		nVal 	= 	{
										n = nVal lsl FractBits.bits
									}

		let to_float 	this	= ( float_of_int (this.n) ) /. float_of_int (1 lsl FractBits.bits)
		let to_int 		this	= this.n lsr FractBits.bits
		let to_string	this	= string_of_float (to_float this)

		let zero 				= of_int 0
		let one					= of_int 1

		let succ		this	= { n = this.n + 1 }
		let pred 		this 	= { n = this.n - 1 }

		let min this 	this2	= if this.n > this2.n then this2 else this
		let max this	this2	= if this2.n > this.n then this2 else this

		let gth	this 	this2	= ( this.n > this2.n )
		let lth	this 	this2	= ( this.n < this2.n )
		let gte	this 	this2	= ( this.n >= this2.n )
		let lte	this 	this2	= ( this.n <= this2.n )
		let eqp	this 	this2 	= ( this == this2 )
		let eqs this 	this2 	= ( this.n = this2.n )

		let add this 	this2 	= of_float ( (to_float this) +. (to_float this2) )
		let sub this 	this2 	= of_float ( (to_float this) -. (to_float this2) )
		let mul this 	this2 	= of_float ( (to_float this) *. (to_float this2) )
		let div this 	this2 	= of_float ( (to_float this) /. (to_float this2) )

		let foreach this this2 f =
			let rec foreachAux n =
				if to_float n = to_float this2 then f n
				else begin f n ; foreachAux (succ n) end
			in
			foreachAux this

	end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	print_endline "(*** SUBJECT TESTS ***)" ;
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f)) ;

	print_endline"\n(*** MY TESTS ***)" ;

	let a = Fixed8.of_int 10 	in
	let b = Fixed8.of_float 2.5	in

	print_endline ( (Fixed8.to_string a) ^ " + " ^ (Fixed8.to_string b) ^ " is " ^ (Fixed8.to_string (Fixed8.add a b)) ) ;
	print_endline ( (Fixed8.to_string a) ^ " - " ^ (Fixed8.to_string b) ^ " is " ^ (Fixed8.to_string (Fixed8.sub a b)) ) ;
	print_endline ( (Fixed8.to_string a) ^ " * " ^ (Fixed8.to_string b) ^ " is " ^ (Fixed8.to_string (Fixed8.mul a b)) ) ;
	print_endline ( (Fixed8.to_string a) ^ " / " ^ (Fixed8.to_string b) ^ " is " ^ (Fixed8.to_string (Fixed8.div a b)) ) ;

	print_newline () ;

	print_endline ( (Fixed8.to_string a) ^ " > " ^ (Fixed8.to_string b) ^ " is " ^ (if (Fixed8.gth a b) then "true" else "false") ) ;
	print_endline ( (Fixed8.to_string a) ^ " < " ^ (Fixed8.to_string b) ^ " is " ^ (if (Fixed8.lth a b) then "true" else "false") ) ;
	print_endline ( (Fixed8.to_string a) ^ " >= " ^ (Fixed8.to_string b) ^ " is " ^ (if (Fixed8.gte a b) then "true" else "false") ) ;
	print_endline ( (Fixed8.to_string a) ^ " <= " ^ (Fixed8.to_string b) ^ " is " ^ (if (Fixed8.lte a b) then "true" else "false") ) ;
	print_endline ( (Fixed8.to_string a) ^ " == " ^ (Fixed8.to_string b) ^ " is " ^ (if (Fixed8.eqp a b) then "true" else "false") ) ;
	print_endline ( (Fixed8.to_string a) ^ " = " ^ (Fixed8.to_string b) ^ " is " ^ (if (Fixed8.eqs a b) then "true" else "false") ) ;
	print_endline ( (Fixed8.to_string a) ^ " = " ^ (Fixed8.to_string a) ^ " is " ^ (if (Fixed8.eqs a a) then "true" else "false") ) ;

	print_newline () ;

	print_endline ( "The min between " ^ (Fixed8.to_string a) ^ " and " ^ (Fixed8.to_string b) ^ " is " ^ (Fixed8.to_string (Fixed8.min a b)) ) ;
	print_endline ( "The max between " ^ (Fixed8.to_string a) ^ " and " ^ (Fixed8.to_string b) ^ " is " ^ (Fixed8.to_string (Fixed8.max a b)) ) ;

	print_newline () ;

	print_endline ( (Fixed8.to_string a) ^ "++ is " ^ (Fixed8.to_string (Fixed8.succ a)) ) ;
	print_endline ( (Fixed8.to_string a) ^ "-- is " ^ (Fixed8.to_string (Fixed8.pred a)) ) ;	

