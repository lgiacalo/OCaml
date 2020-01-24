

module Color = struct

	type t = Spade | Heart | Diamond | Club

	let all = [Spade; Heart; Diamond; Club]

	let toString cc = match cc with
	        | Spade -> "S"
	        | Heart -> "H"
	        | Diamond -> "D"
	        | Club -> "C"
	       
	let toStringVerbose cc = match cc with
	        | Spade -> "Spade"
	        | Heart -> "Heart"
	        | Diamond -> "Diamond"
	        | Club -> "Club"

	
end

module Value = struct

	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

	let toInt v = match v with
	        | T2 -> 1 | T3 -> 2 | T4 -> 3 | T5 -> 4 | T6 -> 5 | T7 -> 6 | T8 -> 7 
	        | T9 -> 8 | T10 -> 9 | Jack -> 10 | Queen -> 11 | King -> 12 | As -> 13

	let toString v = match v with
	        | T2 -> "2" | T3 -> "3" | T4 -> "4" | T5 -> "5" | T6 -> "6" | T7 -> "7" | T8 -> "8"
	        | T9 -> "9" | T10 -> "10" | Jack -> "J" | Queen -> "Q" | King -> "K" | As -> "A"

	let toStringVerbose v = match v with
	        | Jack -> "Jack" | Queen -> "Queen" | King -> "King" | As -> "As"
	        | _ -> toString v
	        
	let next v = 
	        let rec loop lst = match lst with
	                | [] -> invalid_arg "Invalid argument !"
	                | one::two::last when (one = v) -> two
	                | one::last -> loop last
	        in 
	        loop all

	let prev v = 
	        let rec loop lst pred = match lst with
	                | [] -> invalid_arg "Invalid argument"
	                | one::last -> (
	                        if (one = v && pred <> T2) then pred
	                        else loop last one
	                ) 
	        in 
	        loop all T2

	
end

type t = {
	value:Value.t;
	color:Color.t	
}

let newCard v c = {
	value = v;
	color = c
}



(*

val allSpades	: t list
val allHearts	: t list
val allDiamonds	: t list
val allClub		: t list
val all			: t list

val getValue	: t -> Value.t
val getColor	: t -> Color.t

val toString		: t -> string
val toStringVerbose	: t -> string

val compare		: t -> t -> int
val max			: t -> t -> t
val min			: t -> t -> t
val best		: t list -> t

val isOf		: t -> Color.t -> bool
val isSpade		: t -> bool
val isHeart		: t -> bool
val isDiamond	: t -> bool
val isClub		: t -> bool

*)


(*

Nous avons des couleurs et des valeurs, maintenant nous pouvons avoir des cartes! Écrivez le fichier Card.ml qui respecte l'interface ci-dessous. 
Plusieurs choses à noter concernant cette interface:

• Le module Carte intègre les modules Couleur et Valeur. 
Copiez simplement votre code précédent dans les structures correspondantes.

• Le type Card.t est abstrait. Cela signifie que vous êtes libre de l'implémenter comme vous le souhaitez.
Choisissez judicieusement, certaines solutions sont meilleures que les loutres. Et les loutres sont mignonnes.

• Tous les types et identificateurs de valeurs et de fonctions sont explicites. 
Il suffit de lire et d'utiliser votre cerveau, pas de trucs ici.

• La fonction toString: t -> string renvoie des chaînes comme: "2S", "10H", "KD", ...

• La fonction toStringVerbose: t -> string renvoie des chaînes comme: "Card (7, Diamond)", "Card (Jack, Club)", "Card (As, Spade)", ...

• La fonction compare: t -> t -> int se comporte comme la fonction de comparaison Pervasives.

• Les fonctions max et min renvoient le premier paramètre si les deux cartes sont égales.

• La meilleure fonction: t list -> t appelle invalid_arg si la liste est vide. 
Si deux cartes ou plus ont la même valeur, renvoyez la première. 
Les vrais codeurs utilisent List.fold_left pour effectuer cette fonction.


*)










