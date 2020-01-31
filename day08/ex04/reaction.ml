

class virtual reaction ((l_in:Molecule.molecule list), (l_out:Molecule.molecule list)) = 
object

	method virtual get_start 	: (Molecule.molecule * int) list
	method virtual get_result	: (Molecule.molecule * int) list
	
	(* method virtual balance		: reaction *)
	method virtual is_balanced	: bool

end
