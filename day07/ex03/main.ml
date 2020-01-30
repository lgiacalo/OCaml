
let main () = 

	let dal1 = (new Dalek.dalek) in
	let dal2 = (new Dalek.dalek) in
	let dal3 = (new Dalek.dalek) in
	let hum1 = (new People.people "Rose Tyler") in
	let hum2 = (new People.people "Harriet Jones") in
	let hum3 = (new People.people "Jackie Tyler") in
	let doc1 = (new Doctor.doctor "The Doctor One" 700 (new People.people "Rose One")) in
	let doc2 = (new Doctor.doctor "The Doctor Two" 800 (new People.people "Rose Two")) in
	let doc3 = (new Doctor.doctor "The Doctor Three" 900 (new People.people "Rose Three")) in


	print_endline "";
	print_endline "Creation armé de Dalek !";
	let arm1 = (new Army.army [dal1; dal2; dal3]) in
	arm1#print_lst_s;
	arm1#print_lst_t;
	print_endline "Ajout d'un Dalek!";
	let arm11 = arm1#add (new Dalek.dalek) in
	arm11#print_lst_s;
	print_endline "Destruction armee DALEK !";
	(arm11#delete_all)#print_lst_s;

	print_endline "";
	print_endline "Creation armé de People !";
	let arm2 = (new Army.army [hum1; hum2; hum3]) in
	arm2#print_lst_s;
	arm2#print_lst_t;
	print_endline "Ajout d'un People !";
	let arm22 = arm2#add (new People.people "Martha Jones") in
	arm22#print_lst_s;
	print_endline "Destruction armee PEOPLE !";
	((((arm22#delete)#delete)#delete)#delete)#print_lst_s;

	print_endline "";
	print_endline "Creation armé de Doctor ??!";
	let arm3 = (new Army.army [doc1; doc2; doc3]) in
	arm3#print_lst_s;
	print_endline "Ajout d'un Doctor !";
	let arm33 = arm3#add (new Doctor.doctor "The Doctor Four" 1000 (new People.people "Rose Four")) in
	arm33#print_lst_s;
	print_endline "Destruction armee DOCTOR!";
	((((arm33#delete)#delete)#delete)#delete)#print_lst_s


let () = main ()

