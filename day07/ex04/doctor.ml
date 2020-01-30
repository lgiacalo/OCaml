

class doctor name age sidekick = 
object(self)

	val	_name 	 	:string 		= name
	val	_age  		:int			= age
	val	_sidekick	:People.people 	= sidekick

	val  _hp	  	:int			= 100

	initializer			  print_endline "Creation d'une instance de type Doctor."
	method to_string	= "Instance Doctor :\n\tName = " ^ _name ^ "\n\tAge  = " ^ (string_of_int _age) ^ "\n\tHp   = " ^ (string_of_int _hp) ^ "\nSidekick = " ^ sidekick#to_string
	method talk			= print_endline ("Hi! I’m the Doctor!")

	method use_sonic_screwdriver 		= print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
	method travel_in_time start arrival = 
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
		print_endline "\t\t  |  | | | TARDIS| || |       | | |  |";
		print_endline "\t\t  |  | | | TARDIS| || |       | | |  |";
		print_endline "\t\t  |  | | | TARDIS| || |       | | |  |";
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
		print_endline "\t\t'----'----------------------------'----'\n";
		{< _name = _name; _age = (_age + (arrival - start)); _sidekick = _sidekick; _hp = _hp >}
	
	method   	callRegenerate = print_endline "Call methode private regenerate :"; self#regenerate
	method private regenerate  = {< _name = _name; _age = _age ; _sidekick = _sidekick; _hp = 100 >}
	method			reduce_hp x = print_endline ("Hp - Degats " ^ (string_of_int _hp));
								  {< _name = _name; _age = _age ; _sidekick = _sidekick; _hp = (_hp - x) >}

end

