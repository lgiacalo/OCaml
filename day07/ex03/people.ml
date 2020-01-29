

class people name = 
object

	val			_name :string 	= name
	val mutable _hp	  :int		= 100

	initializer			  print_endline "Creation d'une instance de type People."
	method to_string	= "People -> Name : " ^ _name ^ " / Hp : " ^ (string_of_int _hp)
	method talk			= print_endline ("I’m " ^ _name ^ "! Do you know the Doctor?")
	method die			= print_endline ("Aaaarghh!")

end