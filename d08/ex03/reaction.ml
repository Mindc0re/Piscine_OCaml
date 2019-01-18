class virtual reaction (startList: (Molecules.molecule * int) list) (endList: (Molecules.molecule * int) list)  =
object

	method virtual get_start : (Molecules.molecule * int) list

	method virtual get_result : (Molecules.molecule * int) list

	method virtual balance : reaction

	method virtual is_balanced : bool

end