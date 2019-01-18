class virtual atom aName aSymb aNmAt = 
object (this)

	method name : string = aName
	method symbol : string = aSymb
	method atomic_number : int = aNmAt
	method to_string = this#name ^ " : " ^ this#symbol ^ ", " ^ string_of_int this#atomic_number
	method equals (sndAtom : atom) = (this#to_string = sndAtom#to_string)

end

class hydrogen =
object
	inherit atom "Hydrogen" "H" 1
end

class carbon =
object
	inherit atom "Carbon" "C" 6
end

class oxygen =
object
	inherit atom "Oxygen" "O" 8
end

class sodium =
object
	inherit atom "Sodium" "Na" 11
end

class titanium =
object
	inherit atom "Titanium" "Ti" 22
end

class gold =
object
	inherit atom "Gold" "Au" 79
end

class nitrogen	 =
object
	inherit atom "Nitrogen" "N" 7
end

class phosphorus	 =
object
	inherit atom "Phosphorus" "P" 15
end

class sulfur	 =
object
	inherit atom "Sulfur" "S" 16
end

class chlorine	 =
object
	inherit atom "Chlorine" "Cl" 17
end