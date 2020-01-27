(*
A tribute to polyph...poly...you know, that polysleep thingie.
Turn-in directory : ex00/
Files to turn in : micronap.ml
Allowed functions : The Sys and Array modules
*)

let my_sleep () = Unix.sleep 1

let micronap n = 
	let i = ref 1 in
	while !i <= n do
		my_sleep ();
		incr i
	done

let () = 
	if (Array.length Sys.argv) > 1
	then let n = 
		try (int_of_string (Array.get Sys.argv 1)) with
			| _ -> 0
	in micronap n



(* ocamlopt unix.cmxa micronap.ml 

let micronap n = 
	for i = 1 to n do
		my_sleep ()
	done

 *)