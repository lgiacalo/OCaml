(*
Vous allez écrire un programme pour imprimer une blague sur la sortie standard, suivi d'un caractère de fin.

Vos blagues peuvent être ce que vous voulez, mais vous obtenez des points bonus si elles sont mauvaises. 
La seule restriction est que vous les stockerez dans un tableau et qu'il doit y en avoir au moins cinq (5).

Votre programme choisira au hasard une blague dans ce tableau et l'imprimera sur la sortie standard.
Une blague est mauvaise si votre niveleuse veut vous gifler après l'avoir lue.

*)


let safe f in_channel = 
	try Some (f in_channel) 
	with | _ -> None

let nb_lines in_channel  =
	let n = ref (-1) and  i = ref 0 in
	while !i <> !n do
		match (safe input_line in_channel) with
		| None -> n := !i
		| _ -> incr i
	done;
	seek_in in_channel 0;
	!i

let read_jokes in_channel num = 
	 input_line in_channel

let create_jokes in_channel size = 
	let tab = Array.init size (read_jokes in_channel) in
	tab

let () = 
	if (Array.length Sys.argv) > 1
	then begin
	try
		let ic = open_in (Array.get Sys.argv 1) in
		Random.self_init ();
		let nb = (nb_lines ic) in
		let jokes = create_jokes ic nb in
		print_endline (Array.get jokes (Random.int nb))

	with
		| Sys_error err -> print_endline err
		| _ -> ()
	end
	else (print_endline "Not file in argument!")


