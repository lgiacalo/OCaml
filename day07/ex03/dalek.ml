

class dalek = 
object (self)

	val			_name :string 	= (Random.self_init ()); ("Dalek" ^ (String.make 1 (char_of_int ((Random.int 26) + 65))) ^ 
																	(String.make 1 (char_of_int ((Random.int 26) + 65))) ^ 
																	(String.make 1 (char_of_int ((Random.int 26) + 65))))
	val mutable _hp	  :int		= 100
	val mutable _shield:bool	= true


	initializer			print_endline "Creation d'une instance de type Dalek."

	method to_string	= ("Dalek -> Name : " ^ _name ^ " / Hp : " ^ (string_of_int _hp) ^ " / Shield : " ^ (string_of_bool _shield))

	method talk			= match (Random.int 4) with
							| 0 -> print_endline "Explain! Explain!"
							| 1 -> print_endline "Exterminate! Exterminate!"
							| 2 -> print_endline "I obey!"
							| _ -> print_endline "You are the Doctor! You are the enemy of the Daleks!" 

	method exterminate (p:People.people) = p#die ; _shield <- not _shield
	method die			= print_endline ("Emergency Temporal Shift!")


end