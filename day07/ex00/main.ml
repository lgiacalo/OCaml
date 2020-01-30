
let main () = 

	let rose = (new People.people "Rose Tyler") in
	print_endline rose#to_string;
	rose#talk;
	rose#die


let () = main ()