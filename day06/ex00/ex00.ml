(*

val fold_right :('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
	List.fold_right f [a1; ...; an] best f a1 (f a2 (... (f an b) ...)). Pas récursif à la queue.


La bibliothèque STD d'OCaml fournit un module Set. 
Ce module cache une implémentation d'ensembles basés sur des arbres.
Par conséquent, une implémentation aussi efficace des ensembles nécessite 
des éléments ordonnés pour construire l'arborescence interne.
L'implémentation d'un ensemble dépend complètement du type de ses éléments, 
c'est pourquoi les ensembles OCaml sont générés par un foncteur.
Le module Set expose 3 choses:

OrderedType: la signature du paramètre du foncteur.

S: la signature de l'ensemble généré réel.

Make: Le foncteur réel pour créer un ensemble à partir d'un type commandé.
Make : The actual functor to create a set from an ordered type.

Copiez les lignes suivantes dans le fichier "ex00.ml":	
*)

module Test = 
struct
	type t = string
	let compare = compare
end


module StringSet = Set.Make (Test)

(* module StringSet = Set.Make (String) *)

let () =
	let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
	StringSet.iter print_endline set;
	print_endline (StringSet.fold ( ^ ) set "")


(*

	$> ocamlopt ex00.ml && ./a.out
	bar
	baz
	foo
	qux
	quxfoobazbar

*)