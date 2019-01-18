class virtual alkane n = 
object (this)

	method name : string = match n with
	| 1 -> "Methane"
	| 2 -> "Ethane"
	| 3 -> "Propane"
	| 4 -> "Butane"
	| 5 -> "Pentane"
	| 6 -> "Hexane"
	| 7 -> "Heptane"
	| 8 -> "Octane"
	| 9 -> "Nonane"
	| 10 -> "Decane"
	| 11 -> "Undecane"
	| _ -> "Dodecane"

	method formula : string = match n with
	| 1 -> "CH4"
	| 2 -> "C2H6"
	| 3 -> "C3H8"
	| 4 -> "C4H10"
	| 5 -> "C5H12"
	| 6 -> "C6H14"
	| 7 -> "C7H16"
	| 8 -> "C8H18"
	| 9 -> "C9H20"
	| 10 -> "C10H22"
	| 11 -> "C11H24"
	| _ -> "C12H26"

	method to_string = this#name ^ " : " ^ this#formula

	method equals (sndAlkane : alkane) = (this#to_string = sndAlkane#to_string)

end

class met =
object
	inherit alkane 1
end

class eth =
object
	inherit alkane 2
end

class pro =
object
	inherit alkane 3
end

class but =
object
	inherit alkane 4
end

class pen =
object
	inherit alkane 5
end

class hex =
object
	inherit alkane 6
end

class hep =
object
	inherit alkane 7
end

class oct =
object
	inherit alkane 8
end

class non =
object
	inherit alkane 9
end

class dec =
object
	inherit alkane 10
end

class und =
object
	inherit alkane 11
end

class dod =
object
	inherit alkane 12
end