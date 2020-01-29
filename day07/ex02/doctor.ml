

class doctor name age sidekick = 
object

	val	_name 	 		:string 			= name
	val	mutable _age  	:int				= age
	val	_sidekick		:People.people 		= sidekick

	val mutable _hp	  	:int		= 100

	initializer			  print_endline "Creation d'une instance de type Doctor."
	method to_string	= "Doctor -> Name : " ^ _name ^ " / Age : " ^ (string_of_int _age) ^ " / Hp : " ^ (string_of_int _hp) ^ "\nSidekick : " ^ sidekick#to_string
	method talk			= print_endline ("Hi! I’m the Doctor!")

	method use_sonic_screwdriver 		= print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
	method travel_in_time start arrival = 
		_age <- (_age + (arrival - start));
		print_endline "\n\t\t                 _.--._";
		print_endline "\t\t                 _|__|_";
		print_endline "\t\t     _____________|__|_____________";
		print_endline "\t\t  .-'______________________________'-.";
		print_endline "\t\t  | |________POLICE___BOX__________| |";
		print_endline "\t\t  |  |============================|  |";
		print_endline "\t\t  |  | .-----------..-----------. |  |";
		print_endline "\t\t  |  | |  _  _  _  ||  _  _  _  | |  |";
		print_endline "\t\t  |  | | | || || | || | || || | | |  |";
		print_endline "\t\t  |  | | |_||_||_| || |_||_||_| | |  |";
		print_endline "\t\t  |  | | | || || | || | || || | | |  |";
		print_endline "\t\t  |  | | |_||_||_| || |_||_||_| | |  |";
		print_endline "\t\t  |  | |  _______  ||  _______  | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |_______| || |_______| | |  |";
		print_endline "\t\t  |  | |  _______ @||@ _______  | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |_______| || |_______| | |  |";
		print_endline "\t\t  |  | |  _______  ||  _______  | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |       | || |       | | |  |";
		print_endline "\t\t  |  | | |_______| || |_______| | |  |";
		print_endline "\t\t  |  | '-----------''-----------' |  |";
		print_endline "\t\t _|__|/__________________________\\|__|_ ";
		print_endline "\t\t'----'----------------------------'----'\n"

	method private regenerate = (_hp <- 100)

end