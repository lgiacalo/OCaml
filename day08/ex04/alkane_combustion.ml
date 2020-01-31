

(*
class virtual reaction ((l_in:Molecule.molecule list), (l_out:Molecule.molecule list)) = 
object

	method virtual get_start 	: (Molecule.molecule * int) list
	method virtual get_result	: (Molecule.molecule * int) list
	
	method virtual balance		: reaction
	method virtual is_balanced	: bool

end

*)




class alkane_combustion (lst_alk: Alkane.alkane list) = 
object(self)
	inherit Reaction.reaction (((lst_alk :> Molecule.molecule list)@[(new Molecule.dioxygen)]),
								[(new Molecule.water); (new Molecule.carbon_dioxyde)]) as reac

	method  get_start 	= []
	method  get_result	= []
	
	(* method  balance		: reaction *)
	method  is_balanced	= false

end

