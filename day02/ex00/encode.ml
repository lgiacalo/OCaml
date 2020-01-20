
(*  val encode : ’a list -> (int * ’a) list  *)
(*
	Le codage Run-length est une forme très simple d'algorithme de compression de données. 
	Les éléments consécutifs sont stockés en tant qu'élément de données unique et le nombre de fois qu'il se répète. 
	Par exemple, la chaîne "aaabbb" peut être stockée sous "3a3b". 
	Écrivez un encodage de fonction qui encode une liste d'éléments dans une liste de tuples 
	contenant l'élément et le nombre de fois qu'il se répète. 
	La fonction doit être saisie comme suit:
*)

let rec print_list ff lst = match lst with
	| []			-> print_string "\n"
	| tete::queue	-> (
		let (a, b) = tete in
		begin
		print_string "(";
		print_int a;
		print_string ", ";
		ff b;
		print_string "); ";
		print_list ff queue
	end)


let encode liste = match liste with
	| []	-> 	[]
	| _		->	
	begin
		let rec loop lst lstt acc = match lst with
			| first::second::last when first = second -> loop (second::last) lstt (acc + 1)
			| first::last -> loop last (lstt @ [(acc, first)]) 1
			| [] -> lstt
		in
		loop liste [] 1
	end


let () = 

	print_string "[42] = ";
	print_list print_int (encode [42]);

	print_string "1::2::2::1::2::[] = ";
	print_list print_int (encode (1::2::2::1::2::[]));

	print_string "[1; 1; 1; 3; 2; 2; 4; 5; 3] = ";
	print_list print_int (encode [1; 1; 1; 3; 2; 2; 4; 5; 3]);

	print_string "['a', 'a', 'b', 'c', 'c', 'c', 'c', 'd', 'd'] = ";
	print_list print_char (encode ['a'; 'a'; 'b'; 'c'; 'c'; 'c'; 'c'; 'd'; 'd']);

	print_string "[\"string\"; \"string\"; \"string\"; \"int\"; \"int\"; \"string\"; \"float\"; \"float\"; \"float\"; \"float\"; \"float\"] = ";
	print_list print_string (encode ["string"; "string"; "string"; "int"; "int"; "string"; "float"; "float"; "float"; "float"; "float"]);

	print_string "\n"



