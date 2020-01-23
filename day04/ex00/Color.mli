(*

Les cartes à jouer ordinaires conviennent parfaitement comme sujet de programmation lorsque vous traitez des modules et des modules imbriqués. 
Couleurs, valeurs, cartes et jeux, tous liés dans un design intelligent.

Pour commencer, nous devons représenter les couleurs des cartes, à savoir la pelle, le cœur, le diamant et le club, comme un type OCaml et un instrument qui tape avec des valeurs et des fonctions pertinentes.

Fournissez quelques tests dans le fichier main.ml pour prouver que votre module Color fonctionne comme prévu.

 *)

type t = Spade | Heart | Diamond | Club

val all: t list

val toString            : t -> string           (* "S", "H", "D" ou "C" *)
val toStringVerbose     : t -> string
