
let main () = 

	let charles = (new People.people "Charles") in
	print_endline charles#to_string;
	charles#talk;
	charles#die


let () = main ()