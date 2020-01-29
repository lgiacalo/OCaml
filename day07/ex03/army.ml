



class ['a] army (lst: 'a list) = 
object

	val	mutable _lst :'a list 	= lst


	method add l 	= _lst <- _lst@[l]
	method delete 	= match _lst with
						| [] -> ()
						| _ -> _lst <- (List.tl _lst)



	method print_lst_t  = 
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