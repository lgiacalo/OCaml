(*
	Écrivez une fonction qui prend deux listes comme paramètres 
	et renvoie la liste de tous les éléments communs entre les deux listes. 
	La fonction doit être saisie comme suit:

	Dans le cas d'une liste vide comme l'un des paramètres, la fonction doit également renvoyer une liste vide. 
	Mais c’est évident non? Nous n'avons pas à gérer les doublons dans les listes.
*)

(* val crossover : ’a list -> ’a list -> ’a list *)

let rec is_in_list x lst = match lst with
	| []		-> false
	| first::last -> (x = first) || (is_in_list x last)


let crossover list1 list2 = 
	if (list1 = [] || list2 = [])
	then []
	else
	begin
		let rec loop lst lstt = match lst with
			| first::last -> 
				if (is_in_list first list2)
				then (loop last (lstt @ [first]))
				else (loop last lstt)
			| _ -> lstt

		in
		loop list1 []
	end

let rec print_list ff lst = match lst with
	| []			-> print_string "\n"
	| tete::queue	-> (
		begin
		ff tete;
		print_string "; ";
		print_list ff queue
	end)


let () = 
	let l0 = [] in
	let l1 = [1] in
	let l2 = [1; 2; 3; 4; 5] in
	let l3 = [2; 5; 21; 3; 42; 0; 21] in
	let l4 = [1; 2; 2; 3; 3; 3; 21; 3] in
	let l5 = [1; 2; 3; 4; 3; 2; 42] in 

	print_list print_int (crossover l0 l0);
   	print_list print_int (crossover l0 l1);	
   	print_list print_int (crossover l1 l2);
   	print_list print_int (crossover l2 l3);
	print_list print_int (crossover l3 l4);
   	print_list print_int (crossover l4 l5)
