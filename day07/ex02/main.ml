
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

	print_endline "";
	let dal = (new Dalek.dalek) in

	print_endline dal#to_string;
	dal#talk;
	dal#talk;
	let dal1 = (dal#poisoning doct_up) in

	let d = doct_up#reduce_hp 40 in

	let doct_up2 = d#callRegenerate in
	print_endline doct_up2#to_string;

	print_endline "";
	dal1#talk;
	dal1#talk;

	let dal2 = dal1#exterminate rose in
	print_endline dal2#to_string;
	doct_up2#talk;
	dal2#die

let () = main ()

