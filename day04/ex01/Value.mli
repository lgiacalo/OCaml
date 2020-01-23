(*

Nous avons des couleurs, nous avons maintenant besoin de valeurs pour nos cartes. 

Les valeurs des cartes forment un ensemble ordonné total, nous avons besoin d'un type pour les représenter, et de valeurs et de fonctions pour instrumenter ce type. 
Les valeurs des cartes d'un jeu de 52 cartes standard sont 2, 3, 4, 5, 6, 7, 8, 9, 10, valet, reine, roi et as.

 *)


type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

val all : t list

val toInt               : t -> int

val toString            : t -> string
val toStringVerbose     : t -> string

(*      calls invalid_arg      *)
val next        : t -> t
val prev        : t -> t

