(*
Je veux dire, allez, tu sais que je suis beaucoup plus méchant que ça. Faisons maintenant des trucs amusants. 
Dans la prochaine série d'exercices, nous allons essayer de faire un peu d'apprentissage automatique.
Si vous ne savez pas ce qu'est l'apprentissage automatique, recherchez-le sur Wikipédia ou demandez à votre ami entrepreneur hipster NodeJS.

Mais d'abord, nous devons faire les choses de base. Vous écrirez une fonction nommée eu_dist qui prend deux points et calcule la distance euclidienne entre eux.
Si vous ne savez pas quelle est la distance euclidienne, la voici: 
si nous considérons un point comme un tableau de coordonnées a1, a2, a3 ... an et b un autre point comme un tableau de coordonnées b1, b2, b3. ..bn, la distance euclidienne entre a et b est:


Notre modèle pour un point sera un tableau de nombres à virgule flottante, chaque cellule contenant les coordonnées dans une dimension donnée.
Le domaine de votre fonction sera: eu_dist:

Le type de votre fonction sera: float array -> float array -> float

Ne pas gérer des cas avec deux vecteurs de longueurs différentes.

D'accord, vous devez maintenant commencer à comprendre que l'apprentissage automatique n'est pas seulement un mot à la mode. 
C’est surtout des mathématiques. Et ce n’est que le début. Encore avec moi ?

*)

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
	print_endline "|]"



let eu_dist t1 t2 = 
	let len = (Array.length t1) in
	let rec loop n acc = match n with
		| n when (n = len) -> acc
		| _ -> loop (n + 1) (acc +. ((t1.(n) -. t2.(n)) ** 2.))
	in
	sqrt (loop 0 0.0)


let () = 
	
	let tab1 = Array.make 5 1.0 in
	let tab2 = Array.init 5 (fun x -> (float_of_int (x + 1)) *. 2.) in

	print_string "t1 = ";
	print_tab_float tab1;

	print_string "t2 = ";
	print_tab_float tab2;

	print_string "eu_dist t1 t2 = ";
	print_float (eu_dist tab1 tab2);
	print_endline ""


