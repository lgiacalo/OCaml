(*

Cet exercice est ce que les gens appellent habituellement le plaisir, une sorte de distraction entre deux arbres.
Le but principal de cet exercice est de vous présenter le concept de chiffrement très très très basique.
Bien sûr, il va sans dire que le module Cryptokit d'OCaml est strictement interdit pendant tout l'exercice.

- Écrivez une fonction et c'est en face de rot42 et unfrot42 qui prend une chaîne en paramètre et retourne la chaîne en tournant 42 de tout son caractère.

- Écrivez une fonction et c'est opposé césar et uncaesar qui prend une chaîne et un int comme paramètres et retourne la chaîne en tournant tout son caractère vers la droite selon l'int (comme un rotn en fait).

- Ecrivez une fonction xor qui prend une chaîne et une clé int comme paramètres et retourne la chaîne en xorant tout son caractère avec la clé. Cette fonction est son propre contraire.

- Écrivez une fonction et c'est opposé à ft_crypt et ft_uncrypt qui prend une chaîne et une liste de fonctions de type césar ou xor comme paramètres et retourne la nouvelle chaîne après l'application des fonctions. 
Bien sûr, enveloppez tout dans un programme pour prouver que votre travail fonctionne.

- Toutes les fonctions de chiffrement doivent aller dans le fichier "cipher.ml" et toutes les fonctions de dechiffrement doivent aller dans le fichier "uncipher.ml".

*)

let ft_is_upper c = 
        c <= 'Z' && c >= 'A'
let ft_is_lower c = 
        c <= 'z' && c >= 'a'

let caesar str n = 
        let rot = (n mod 26) in
        let ft_rot c = 
                let ascii = int_of_char c in
                if (ft_is_upper c)
                then
                        if (ascii + rot > (int_of_char 'Z'))
                        then char_of_int ((int_of_char 'A') + ascii + rot - (int_of_char 'Z'))
                        else char_of_int (ascii + rot)
                else if (ft_is_lower c)
                then
                        if (ascii + rot > (int_of_char 'z'))
                        then char_of_int ((int_of_char 'a') + ascii + rot - (int_of_char 'z'))
                        else char_of_int (ascii + rot)
                else c
         in
         String.map ft_rot str


let rot42 str = caesar str 42

let xor str n = 
        let ft_xor c =
                let nb = int_of_char c in
                let res = (nb lxor n) in
                char_of_int res
        in 
        String.map ft_xor str






let main () = 
        print_endline "Caesar : ";

        print_string "1 -> abcdefghijklmnopqrstuvwxyz :";
        print_string ((caesar "abcdefghijklmnopqrstuvwxyz" 1) ^ " : ");
        print_string "13 -> abcdefghijklmnopqrstuvwxyz :";
        print_string ((caesar "abcdefghijklmnopqrstuvwxyz" 13) ^ "\n");
        print_string "42 -> 0123456789 :";
        print_string ((caesar "0123456789" 42) ^ "\n");
        print_string "2 -> OI2EAS67B9 : ";
        print_string ((caesar "OI2EAS67B9" 2) ^ "\n");
        print_string "0 -> Damned ! :";
        print_string ((caesar "Damned !" 0) ^ "\n");
        print_string "42 -> [] :";
        print_string ((caesar "" 42) ^ "\n");
        print_string "1 -> NBzlk qnbjr ! :";
        print_string ((caesar "NBzlk qnbjr !" 1) ^ "\n");


        print_endline "\nXor : ";
        print_string "1 -> abcdefghijklmnopqrstuvwxyz :";
        let res =  (xor "abcdefghijklmnopqrstuvwxyz" 1) in
        print_endline res;
        print_string "1 -> ";
        print_string res;
        print_string " :";
        print_endline (xor res 1);

        print_endline "\n\nend"

let () = main ()
