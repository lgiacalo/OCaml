(*
Version 4.08.0
Dans cet exercice, vous allez écrire une fonction nommée examples_of_file 
qui prend un chemin d'accès à un fichier comme argument 
et retourne un ensemble d'exemples lus à partir de l'entrée du fichier, 
qui est formaté en csv.
*)
(*	examples_of_file:  string -> (float array * string) list	*)


let print_tab_float tab = 
	let len = (Array.length tab) in
	print_string "[|";
	let rec loop n = match n with
		| n when (n = len) -> ()
		| n when (n + 1 = len) -> (
			print_float tab.(n);
			loop (n + 1)
		)
		| _ -> (
			print_float tab.(n);
			print_string "; ";
			loop (n + 1)
		)
	in
	loop 0;
	print_string "|]"

let print_examples_of_file lst = 
	print_string "[ ";
	let rec loop lst = match lst with
		| [] -> print_endline ""
		| (tab, str)::t -> (
			print_string "(";
			print_tab_float tab;
			print_endline (", " ^ str ^ "); ");
			loop t
		)
	in
	loop lst;
	print_endline " ]"

let safe f in_channel = 
	try Some (f in_channel) 
	with | _ -> None

let func1 line c = 
	let lst = (String.split_on_char c line) in
	let len = (List.length (lst)) - 1 in
	let f1 lst n = float_of_string (List.nth lst n) in
	let tab = (Array.init len (f1 lst)) in
	(tab, (List.nth lst len))

let examples_of_file file = 
	let ic = (open_in file) in
	let rec loop llst = match (safe input_line ic) with
		| None -> close_in ic; llst
		| Some (_ as line) -> loop (llst@[(func1 line ',')])
	in 
	loop []

let () = 

	if (Array.length Sys.argv) > 1
	then (
		let ret = examples_of_file (Array.get Sys.argv 1) in
		print_examples_of_file ret

	)
	else (print_endline "Not file in argument!")

