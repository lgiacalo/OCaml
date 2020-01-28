
module Test = 
struct
	type t = string

	let equal s1 s2 = (s1 = s2)

	let hash s = 
		let rec loop len nb = match len with
			| -1 -> nb
			| x -> loop (len - 1) (((int_of_char (String.get s x)) + nb) lxor (nb lsr 1))
		in
		(loop ((String.length s) - 1) 0xfff0) + (String.length s)
end


module StringHashtbl = Hashtbl.Make (Test)


let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
	List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
	StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht



(*
	$> ocamlopt ex01.ml && ./a.out
	k = "Ocaml", v = 5
	k = "Hello", v = 5
	k = "42", v = 2
	k = "H", v = 1
	k = "world", v = 5
*)