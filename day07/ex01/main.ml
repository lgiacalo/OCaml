
let main () = 

	let rose = (new People.people "Rose Tyler") in
	print_endline rose#to_string;
	rose#talk;
	rose#die;

	print_endline "";
	let doct = (new Doctor.doctor "The Doctor" 900 rose) in
	print_endline doct#to_string;
	doct#talk;

	let doct_up = doct#travel_in_time 2000 2050 in
	print_endline doct_up#to_string;
	doct_up#use_sonic_screwdriver;

	print_endline (doct_up#callRegenerate)#to_string


let () = main ()

