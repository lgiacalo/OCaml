(*

Comme vu précédemment, deux hélices peuvent se combiner pour créer une structure d'ADN.
Comme vous le verrez dans cet exercice, les règles sont appliquées en cas de combinaison.
Le lien de cette combinaison se produit là où les bases sont situées:
P - D - Base <=> Base - D - P, P - D - Base <=> Base - D - P, ....

- Écrivez un type Helix qui est une liste d'éléments de type Nucleotide.

- Écrire une fonction generate_helix qui prend un int n comme paramètre
et construire une séquence aléatoire de nucléotides comme une liste de taille n.

- Écrivez une fonction helix_to_string qui convertit une liste de nucléotides de type Helix 
résultant de la fonction précédente en une chaîne de nucléobases.

- Écrire une fonction complementaire_helix qui prend un Helix comme paramètre et 
génére un Helix correspondante selon les règles d'appariement de la nucléobase qui suit:

- A (Ademine) peut être associé à T.
- T (Thymine) peut être associé à A.
- C (Cytosine) peut être associé à G.
- G (Guanine) peut être associé à C.

La fonction doit être saisie comme suit:
val complementaire_helix: hélice -> hélice 
 
 *)

(********************************* TYPES *********************************************)

type phosphate          = string
type deoxyribose        = string
type nucleobase         = A | T | C | G | None

type nucleotide         = {
        p       : phosphate;
        d       : deoxyribose;
        base    : nucleobase
}

type helix              = nucleotide list


(********************************* PRINTS *********************************************)

let print_nucleobase base = match base with
        | A -> print_string "A"
        | T -> print_string "T"
        | C -> print_string "C"
        | G -> print_string "G"
        | _ -> print_string "None"

let print_nucleotide nucl = 
        print_string "phosphate   : "; print_endline nucl.p;
        print_string "deoxyribose : "; print_endline nucl.d;
        print_string "nucleobase  : "; print_nucleobase nucl.base;
        print_char '\n'

let print_helix lst = match lst with
        | [] -> print_endline "Liste de nucleotide vide"
        | _ -> (
                print_endline "\n********** HELIX ***********";
                let rec loop lst = match lst with
                        | [] -> ()
                        | first::last -> (
                                print_nucleotide first;
                                loop last
                        )
                in
                loop lst;
                print_endline "****************************\n";
        )


(********************************* CONVERSION *****************************************)

let nucleobase_to_string base = match base with
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | _ -> "_"


let helix_to_string hel = match hel with
        | [] -> ""
        | _ -> (
                let rec loop hel str = match hel with
                        | [] -> str
                        | first::last -> (
                                loop last (str ^ (nucleobase_to_string first.base))
                )
                in 
                loop hel ""
        )

(********************************* FONCTIONS ******************************************)

let generate_nucleobase_random = match (Random.self_init () ; Random.int 4) with
        | 0 -> 'A' | 1 -> 'T' | 2 -> 'C' | 3 -> 'G' | _ -> '0'


let generate_nucleotide c = {
                p       = "phosphate";
                d       = "deoxyribose";
                base    = match c with
                        | 'A' -> A | 'T' -> T | 'C' -> C | 'G' -> G | _ -> None
}

let generate_helix n = match n with
        | _ when n <= 0 -> []
        | _ -> (
                let rec loop n lst = match n with
                        | 0 -> lst
                        | _ -> loop (n - 1) (lst @ [generate_nucleotide (generate_nucleobase_random)])
                in
                loop n []
)

let complementary_helix hel = match hel with
        | [] -> []
        | _ -> (
                let rec loop hel ret = match hel with
                        | [] -> ret
                        | first::last -> loop last (ret @ [generate_nucleotide (
                                match first.base with
                                        | A -> 'T'
                                        | T -> 'A'
                                        | G -> 'C'
                                        | C -> 'G'
                                        | None -> '_'
                )])
                in
                loop hel []
        )



(********************************* MAIN ******************************************)

let () = 
        print_nucleotide (generate_nucleotide 'A');
        print_nucleotide (generate_nucleotide 'T');
        print_nucleotide (generate_nucleotide 'C');
        print_nucleotide (generate_nucleotide 'G');
        print_nucleotide (generate_nucleotide 'B');

        
        print_nucleotide (generate_nucleotide (generate_nucleobase_random));
        print_nucleotide (generate_nucleotide (generate_nucleobase_random));
        print_nucleotide (generate_nucleotide (generate_nucleobase_random));

        print_helix (generate_helix 2);
        print_helix (generate_helix 3);
        print_helix (generate_helix 5);

        print_endline (helix_to_string (generate_helix 4));

        let hel = generate_helix 5 in
        print_helix hel;
        print_helix (complementary_helix hel);

        let hell = generate_helix 2 in
        print_helix hell;
        print_helix (complementary_helix hell)





