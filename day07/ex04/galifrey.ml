
class galifrey = 
object(this)


	val	 _lst_dalek  :Dalek.dalek 	list 	= []
	val	 _lst_doctor :Doctor.doctor list 	= []
	val	 _lst_people :People.people list 	= []

	initializer			print_endline "Creation d'une instance de type Galifrey."


	method add_dalek (d:Dalek.dalek) = {< _lst_dalek = _lst_dalek@[d]; _lst_doctor = _lst_doctor; _lst_people = _lst_people >}
	method add_dalek_l lst_dalek 	 = {< _lst_dalek = lst_dalek; _lst_doctor = _lst_doctor; _lst_people = _lst_people >}
	method creation_dalek n 		 = print_endline ("\nCreation de " ^ (string_of_int n) ^ " Dalek :");
								let rec loop n (lst:Dalek.dalek list) = match n with
									| 0 -> (this#add_dalek_l lst)
									| _ -> loop (n - 1) (lst@[new Dalek.dalek])
								in loop n []

	method add_doctor 	= print_endline ("\nCreation du Doctor :");
						{< _lst_dalek = _lst_dalek; _lst_doctor = _lst_doctor@[(new Doctor.doctor "The Doctor" 700 (new People.people "Rose Tyler"))]; _lst_people = _lst_people >}

	method add_people (p:People.people) = {< _lst_dalek = _lst_dalek; _lst_doctor = _lst_doctor; _lst_people = _lst_people@[p] >}
	method add_people_l lst_people 		= {< _lst_dalek = _lst_dalek; _lst_doctor = _lst_doctor; _lst_people = lst_people >}
	method creation_people n lst_name	= print_endline ("\nCreation de " ^ (string_of_int n) ^ " People :");
								let rec loop n (lst:People.people list) = match n with
									| 0 -> (this#add_people_l lst)
									| _ -> loop (n - 1) (lst@[new People.people (List.nth lst_name (n - 1))])
								in loop n []

	method do_time_war = 
		print_endline "\n\tPlanet Galifrey - C'est la guerre a travers le temps et l'espace \\o/!!\n";
		let rec war_loop (dal, doc, peo) =
			match (dal, doc, peo) with
				  ([], _, []) -> "Arg! Tout le monde est mort..."
			 	| (_, _, []) -> "Victoire des Daleks!"
			 	| ([], _, _) -> "The doctor a gagné la guerre \\o/!!"
			 	| (hda::tda, dr, hpe::tpe) -> 
			doc#talk; doc#use_sonic_screwdriver;
			hpe#talk;
			hda#talk; hda#exterminate hpe; hda#die;
			print_char '\n';
			war_loop (tda, dr, tpe)
		in print_endline (war_loop (_lst_dalek, (List.hd _lst_doctor), _lst_people))

end