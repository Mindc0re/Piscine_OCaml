class virtual molecule mName aList = 
object (this)

	val _atomList = ref aList

	method private hillList (l: Atoms.atom list) = 
		let carbonAtoms = List.filter (fun a -> a#atomic_number = 6) l in
		let hydrogenAtoms = List.filter (fun a -> a#atomic_number = 1) l in
		let sortedList = List.sort (fun a b -> String.compare a#symbol b#symbol) l in
		let noCarbonAndHydrogenList = List.filter (fun a -> a#atomic_number <> 1 && a#atomic_number <> 6) sortedList in

		if carbonAtoms = [] && hydrogenAtoms = [] then sortedList
		else if carbonAtoms = [] && hydrogenAtoms <> [] then (hydrogenAtoms @ noCarbonAndHydrogenList)
		else if carbonAtoms <> [] && hydrogenAtoms = [] then (carbonAtoms @ noCarbonAndHydrogenList)
		else carbonAtoms @ hydrogenAtoms @ noCarbonAndHydrogenList

	method name : string = mName

	method formula : string = 
		if !_atomList = [] then ""
		else
		let rec formulaAux (accStr:string) (prevAtom:string) (accNbAtom:int) (l:Atoms.atom list) = 
			match l with
			| hd::tl when tl = [] 
				->
					if hd#symbol = prevAtom then (accStr ^ prevAtom ^ (string_of_int (accNbAtom + 1)))
					else (accStr ^ prevAtom ^ (if accNbAtom <> 1 then (string_of_int accNbAtom) else "") ^ hd#symbol)
			| hd::tl when hd#symbol = prevAtom
				-> formulaAux accStr prevAtom (accNbAtom + 1) tl
			| hd::tl when hd#symbol <> prevAtom
				-> formulaAux (accStr ^ prevAtom ^ if accNbAtom <> 1 then (string_of_int accNbAtom) else "") hd#symbol 1 tl
			| _ -> accStr
		in
		formulaAux "" (List.hd !_atomList)#symbol 0 !_atomList

	method to_string = this#name ^ " : " ^ this#formula

	method equals (sndMolecule : molecule) = (this#to_string = sndMolecule#to_string)

	initializer _atomList := this#hillList !_atomList

end

class water =
object
	inherit molecule "Water" [new Atoms.hydrogen ; new Atoms.oxygen ; new Atoms.hydrogen]
end

class carbonDioxyde =
object
	inherit molecule "Carbon dioxyde" [new Atoms.oxygen ; new Atoms.oxygen ; new Atoms.carbon]
end

class tnt =
object
	inherit molecule "Trinitrotoluene"	[
											new Atoms.nitrogen ; new Atoms.nitrogen ; new Atoms.nitrogen ; 
											new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; 
											new Atoms.oxygen ; new Atoms.oxygen ; new Atoms.oxygen ; new Atoms.oxygen ; new Atoms.oxygen ; new Atoms.oxygen ; 
											new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon 
										]
end

class chl =
object
	inherit molecule "Chlormephos"	[
											new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ;
											new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; 
											new Atoms.chlorine ; 
											new Atoms.oxygen ; new Atoms.oxygen ;
											new Atoms.phosphorus ;
											new Atoms.sulfur ; new Atoms.sulfur 
										]
end

class pen =
object
	inherit molecule "Pentane"	[
											new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; new Atoms.carbon ; 
											new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.hydrogen ; 
										]
end