(*
Vous allez écrire un programme pour imprimer une blague sur la sortie standard, suivi d'un caractère de fin.

Vos blagues peuvent être ce que vous voulez, mais vous obtenez des points bonus si elles sont mauvaises. 
La seule restriction est que vous les stockerez dans un tableau et qu'il doit y en avoir au moins cinq (5).

Votre programme choisira au hasard une blague dans ce tableau et l'imprimera sur la sortie standard.
Une blague est mauvaise si votre niveleuse veut vous gifler après l'avoir lue.

*)

let les_blagues n = match n with
	| 0 -> "Que fait une fraise sur un cheval ? Tagada Tagada."
	| 1 -> "C'est l'histoire de l'eunuque décapité. Une histoire sans queue ni tête."
	| 2 -> "C'est l'histoire d'un pingouin qui respire par les fesses. Un jour il s’assoit et il meurt."
	| 3 -> "Pourquoi les canards sont toujours à l'heure ? Parce qu’ils sont dans l’étang."
	| _ -> "C'est quoi un petit pois avec une épée face à une carotte avec une épée ? Un bon duel."


let () = 
	Random.self_init ();

	let blagues = (Array.init 5 les_blagues) in
	print_endline (Array.get blagues (Random.int 5))

