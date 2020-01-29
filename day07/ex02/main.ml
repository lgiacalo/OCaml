
let main () = 

	let rose = (new People.people "Rose Tyler") in
	print_endline rose#to_string;
	rose#talk;

	print_endline "";
	let doct = (new Doctor.doctor "The Doctor" 900 rose) in
	print_endline doct#to_string;
	doct#talk;

	doct#travel_in_time 2000 2050;
	print_endline doct#to_string;
	doct#use_sonic_screwdriver;

	print_endline "";
	let dal = (new Dalek.dalek) in

	print_endline dal#to_string;
	print_endline doct#to_string;
	dal#talk;
	dal#talk;

	dal#exterminate rose;
	print_endline dal#to_string;
	doct#talk;
	dal#die

let () = main ()

