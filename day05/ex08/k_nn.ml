
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

(**************************************************************************)

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

let eu_dist t1 t2 = 
	let len = (Array.length t1) in
	let rec loop n acc = match n with
		| n when (n = len) -> acc
		| _ -> loop (n + 1) (acc +. ((t1.(n) -. t2.(n)) ** 2.))
	in
	sqrt (loop 0 0.0)

(**************************************************************************)

(* 
	one_nn: radar list -> radar -> string 

	val fst :'a * 'b -> 'a
	val snd :'a * 'b -> 'b
*)

type radar = float array * string

let one_nn lst_r r = 
	let rec loop lst nb str = match lst with
		| [] -> str
		| (arr, s)::t -> (
			let dif = (eu_dist arr (fst r)) in
			if (dif <= nb) then loop t dif s
			else loop t nb str
		)
	in
	let diff = (eu_dist (fst (List.hd lst_r)) (fst r)) in
	loop lst_r diff ""

(**************************************************************************)

(*	k_nn: radar list -> int -> radar -> string 		*)


(*
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
	List.fold_left f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) bn.
*)

let inc_list lst el = 
	let rec loop l ll = match l with
		| [] -> ll
		| (diff, c)::t when ((fst el) < diff) -> loop t (ll@[el])
		| h::t -> loop t ll@[h]
	in
	loop lst []

(* let k_nn lst_r nb r = 
	let rec loop
 *)



(* 
let () =
 	 print_string "Test csv ex06 : ";
 	 print_endline (one_nn (examples_of_file "../ex06/ionosphere.test.csv")
		([| 1.;0.;0.74916;0.02549;0.98994;0.09792;0.75855;0.12877;0.74313;-0.09188;0.95842;0.02482;0.97921;-0.00469;0.96110;0.10195;0.91482;0.03756;0.71026;0.02683;0.81221;-0.08048;1.;0.;0.71764;-0.01207;0.82271;0.02552;0.72435;-0.01073;0.90409;0.11066;0.72837;0.02750|], "g"));

 *)


