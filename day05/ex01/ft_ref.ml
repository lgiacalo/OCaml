(*
	A tribute to something you should normally never use.
Turn-in directory : ex01/
Files to turn in : ft_ref.ml
Forbidden functions : ref
*)

(*
Créez un type ft_ref pour reproduire le type ref et implémentez les fonctions suivantes:

• return: ’a ->’ a ft_ref: crée une nouvelle référence.

• get: ’a ft_ref ->’ a: Déréférence une référence.

• set: ’a ft_ref ->’ a -> unit: attribue la valeur d’une référence.

• bind: ’a ft_ref -> (’ a -> ’b ft_ref) ->’ b ft_ref:
Celui-ci est un peu plus compliqué. Il applique une fonction à une référence pour la transformer. 
Vous pouvez le voir comme une fonction d'ensemble plus compliquée.

L'utilisation du type standard ref est évidemment interdite, mais jouer avec lui dans l'interpréteur devrait vous dire comment il est implémenté en interne. 
Votre objectif est de faire la même chose. Oh, et au fait, après cet exercice, vous aurez fait votre première monade.
Les monades sont une sorte de magie noire ancienne avec laquelle vous pourrez jouer assez tôt. Rendez-vous le d09 ...
*)

type 'a ft_ref = { mutable content : 'a }

(*Pour copier ref, cette fonction aurait du sappeler ft_ref*)
let return a = 
	let (new_ref: 'a) = {content = a} in
	new_ref

let get r = r.content

let set r a = r.content <- a

let function_test a = 
	return (a * 3)

let bind r f = 
	f r.content

let () = 
	
	let a = return 42 in

	print_endline "Ft_ref : ";
	print_endline "Nouvelle variable a de type ft_ref : a = return 42 ";
	print_endline ("\t a = { content = " ^ (string_of_int (get a)) ^ " }\n");
	print_endline "Modification variable a : set a 21";
	set a 21;
	print_endline ("\t a = { content = " ^ (string_of_int (get a)) ^ " }\n");
	print_endline "Bind de a avec la fonction (x -> x * 3)";
	print_endline ("\t a = { content = " ^ (string_of_int (get (bind a function_test))) ^ " }\n")


