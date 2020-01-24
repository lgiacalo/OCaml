
module Color : 
sig

	type t = Spade | Heart | Diamond | Club

	val all: t list

	val toString            : t -> string           (* "S", "H", "D" ou "C" *)
	val toStringVerbose     : t -> string

	
end

module Value : 
sig

	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	val all : t list

	val toInt               : t -> int

	val toString            : t -> string
	val toStringVerbose     : t -> string

	(*      calls invalid_arg      *)
	val next        : t -> t
	val prev        : t -> t
	
end

type t = {
	value: Value.t;
	color: Color.t
	
}

val newCard : Value.t -> Color.t -> t

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










