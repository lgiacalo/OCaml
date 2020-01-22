
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


let uncaesar n str = 
        let rot = 26 - (n mod 26) in
        Cipher.caesar rot str

let unrot42 str = uncaesar 42 str

let ft_uncrypt str lst = match lst with
        | [] -> str
        | _ -> (
                let rec reverse lst llst = match lst with
                        | [] -> llst
                        | f::last -> reverse last ([f] @ llst)
                in
                let new_list = reverse lst [] in
                Cipher.ft_crypt str new_list
        )



(*********************************************************************************************************)


let main () = 
        print_endline "Rot42 : ";
        print_string "abcdefghijklmnopqrstuvwxyz : ";
        print_string ((Cipher.rot42 "abcdefghijklmnopqrstuvwxyz") ^ " : ");
        print_endline (unrot42 (Cipher.rot42 "abcdefghijklmnopqrstuvwxyz"));

        print_endline "\nCaesar : ";
        print_string "1 -> abcdefghijklmnopqrstuvwxyz : ";
        print_string ((Cipher.caesar 1 "abcdefghijklmnopqrstuvwxyz") ^ " : ");
        print_endline (uncaesar 1 (Cipher.caesar 1 "abcdefghijklmnopqrstuvwxyz"));

        print_string "13 -> abcdefghijklmnopqrstuvwxyz : ";
        print_string ((Cipher.caesar 13 "abcdefghijklmnopqrstuvwxyz") ^ " : ");
        print_string ((uncaesar 13 (Cipher.caesar 13 "abcdefghijklmnopqrstuvwxyz")) ^ "\n");

        print_string "42 -> 0123456789 : ";
        print_string ((Cipher.caesar 42 "0123456789") ^ " : ");
        print_string ((uncaesar 42 (Cipher.caesar 42 "0123456789")) ^ "\n");

        print_string "2 -> OI2EAS67B9 : ";
        print_string ((Cipher.caesar 2 "OI2EAS67B9") ^ " : ");
        print_string ((uncaesar 2 (Cipher.caesar 2 "OI2EAS67B9")) ^ "\n");

        print_string "0 -> Damned ! : ";
        print_string ((Cipher.caesar 0 "Damned !") ^ " : ");
        print_string ((uncaesar 0 (Cipher.caesar 0 "Damned !")) ^ "\n");

        print_string "42 -> [] : ";
        print_string ((Cipher.caesar 42 "") ^ " : ");
        print_string ((uncaesar 42 (Cipher.caesar 42 "")) ^ "\n");

        print_string "1 -> NBzlk qnbjr ! : ";
        print_string ((Cipher.caesar 1 "NBzlk qnbjr !") ^ " : ");
        print_string ((uncaesar 1 (Cipher.caesar 1 "NBzlk qnbjr !")) ^ "\n");


        print_endline "\nXor : ";
        print_string "1 -> abcdefghijklmnopqrstuvwxyz : ";
        let res =  (Cipher.xor 1 "abcdefghijklmnopqrstuvwxyz") in
        print_endline res;
        print_endline ("1 -> " ^ res ^ " :" ^ (Cipher.xor 1 res));

        
        print_endline "\nFt_crypt : ";
        print_endline ("Rot42 -> abcd : " ^ (Cipher.ft_crypt "abcd" [Cipher.rot42 ; unrot42]));

        print_string ("\"Jwnnm*}mfv*+\" ; [uncaesar 8; xor 10] -> ");
        print_string ((Cipher.ft_crypt "Jwnnm*}mfv*+" [uncaesar 8; Cipher.xor 10]) ^ " : ");
        print_endline ((ft_uncrypt (Cipher.ft_crypt "Jwnnm*}mfv*+" [uncaesar 8; Cipher.xor 10]) [Cipher.caesar 8; Cipher.xor 10]));

        print_string "\"Damned ! : \" [rot42; xor 12; caesar 13] -> ";
        print_string ((Cipher.ft_crypt "Damned !" [Cipher.rot42; Cipher.xor 12; Cipher.caesar 13]) ^ " : ");
        print_endline ((ft_uncrypt (Cipher.ft_crypt "Damned !" [Cipher.rot42; Cipher.xor 12; Cipher.caesar 13]) [unrot42; Cipher.xor 12; uncaesar 13]));
        print_endline "\n"


let () = main ()

