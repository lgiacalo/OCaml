
class ['a] army (lst: 'a list) = 
object(this)

	val	_lst :'a list 	= lst

	initializer			print_endline "Creation d'une instance de type Army."

	method get		= _lst

	method add l 	= {< _lst = (_lst@[l]) >}
	method delete 	= match _lst with
						| [] -> this
						| _ -> {< _lst = (List.tl _lst) >}

	method delete_all = {< _lst = [] >}

	method print_lst_t  = if (_lst = []) then print_endline "Armée vide" else
		let rec loop lst = match lst with
			| [] 	-> ()
			| h::t 	-> h#talk; loop t
		in loop _lst

	method print_lst_s  = if (_lst = []) then print_endline "Armée vide" else
		let rec loop lst = match lst with
			| [] 	-> ()
			| h::t 	-> print_endline h#to_string; loop t
		in loop _lst

end