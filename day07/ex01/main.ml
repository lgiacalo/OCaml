
let main () = 

	let rose = (new People.people "Rose Tyler") in
	print_endline rose#to_string;
	rose#talk;
	rose#die;

	print_endline "";
	let doct = (new Doctor.doctor "The Doctor" 900 rose) in
	print_endline doct#to_string;
	doct#talk;

	doct#travel_in_time 2000 2050;
	print_endline doct#to_string;
	doct#use_sonic_screwdriver



let () = main ()

