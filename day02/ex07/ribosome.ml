
(*

Le ribosome est une machine moléculaire grande et complexe présente dans toutes les cellules vivantes.
Son objectif principal est de créer des protéines à partir d'un ARN messager en combinant ensemble des acides aminés. 
Les protéines sont essentielles à tous les organismes vivants, y compris les humains.

- Ecrire une fonction generate_bases_triplets qui crée 
une liste de triplets d'éléments de type nucléobase à partir d'un élément de type rna selon la règle suivante:

- si le nombre de nucléobases de la liste n'est pas un multiple de 3, il ignore le dernier triplet incomplet.
- La fonction doit être de type:
generate_bases_triplets: liste rna -> (nucléobase * nucléobase * nucléobase) list.

- Écrivez un type de protéine qui se compose d'une liste d'acides aminés et 
de la fonction string_of_protein de type protein -> string.

- Fonction d'écriture decode_arn de type rna -> protéine qui crée 
une liste du variant d'acides aminés de type à partir d'un élément de type rna selon les règles suivantes:

- Le processus de décodage commence avec le premier triplet et se termine avec le premier triplet d'arrêt rencontré. 
N'est-ce pas évident?

- Voici le tableau de correspondance du triplet de base nucléique, de l'animide acide correspondant et 
du constructeur de type acide aminé:

 *)

(********************************* TYPES *********************************************)

type phosphate          = string
type deoxyribose        = string
type nucleobase         = A | T | C | G | U | None

type nucleotide         = {
        p       : phosphate;
        d       : deoxyribose;
        base    : nucleobase
}

type helix              = nucleotide list
type rna                = nucleobase list

type aminoacid          = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His |
                           Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr |
                           Val

type protein            = aminoacid list

(********************************* PRINTS *********************************************)

let string_of_aminoacid (am:aminoacid) = match am with
        | Stop  -> "Stop"
        | Ala   -> "Alamine"
        | Arg   -> "Arginine"
        | _     -> "a voir"


let string_of_protein (prot:protein) = match prot with
        | [] -> ""
        | _ -> (
                let rec loop prot str = match prot with
                        | [] -> str
                        | first::last -> loop last (str ^ (string_of_aminoacid first))
                in
                loop prot ""
)


let print_nucleobase (base:nucleobase) = match base with
        | A -> print_string "A"
        | T -> print_string "T"
        | C -> print_string "C"
        | G -> print_string "G"
        | U -> print_string "U"
        | _ -> print_string "None"

let print_nucleotide (nucl:nucleotide) = 
        print_string "phosphate   : "; print_endline nucl.p;
        print_string "deoxyribose : "; print_endline nucl.d;
        print_string "nucleobase  : "; print_nucleobase nucl.base;
        print_char '\n'

let print_helix (lst:helix) = match lst with
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
                print_endline "****************************\n"
        )

let print_rna (r:rna) = match r with
        | [] -> print_endline "RNA: Liste de nucleobase vide"
        | _ -> (
                print_endline "\n********** RNA ***********";
                let rec loop lst = match lst with
                        | [] -> ()
                        | first::last -> (
                                print_nucleobase first;
                                print_string ";";
                                loop last
                        )
                in
                loop r;
                print_endline "\n**************************\n"
        )

let print_triplet_base lst = match lst with
        | [] -> print_endline "Liste Triple nucleobase Vide"
        | _ -> (
                print_endline "\n******** TRIPLET BASE *********";
                let rec loop lst = match lst with
                        | first::last -> (
                                let (a, b, c) = first in
                                print_nucleobase a;
                                print_nucleobase b;
                                print_nucleobase c;
                                print_string ";";
                                loop last
                        )
                        | _ -> ()
                in
                loop lst;
                print_endline "\n********************************\n"
        )


(********************************* CONVERSION *****************************************)

let nucleobase_to_string (base:nucleobase) = match base with
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | U -> "U"
        | _ -> "_"


let helix_to_string (hel:helix) = match hel with
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

let complementary_helix (hel:helix) = match hel with
        | [] -> []
        | _ -> (
                let rec loop hel ret = match hel with
                        | [] -> ret
                        | first::last -> loop last (ret @ [generate_nucleotide (
                                match first.base with
                                        | A -> 'T' | T -> 'A' | G -> 'C' | C -> 'G' | _ -> '_'
                )])
                in
                loop hel []
        )

let rna_nucleobase_pairing (base:nucleobase) = match base with
        | A -> U
        | T -> A
        | C -> G
        | G -> C
        | _ -> None

let generate_rna (hel:helix) = match hel with
        | [] -> []
        | _ -> (
                let rec loop hel lst = match hel with
                        | [] -> lst
                        | first::last -> loop last (lst @ [(rna_nucleobase_pairing first.base)])
                in
                loop hel []
)

let generate_bases_triplets (r:rna) = match r with
        | [] -> []
        | _ -> (
                let rec loop r lst = match r with
                        | first::second::third::last -> loop last (lst @ [(first, second, third)])
                        | _ -> lst
                in
                loop r []
)

(********************************* MAIN ******************************************)

let () = 

        print_endline (helix_to_string (generate_helix 2));

        let hell = generate_helix 2 in
        print_helix hell;
        print_helix (complementary_helix hell);

        let base:nucleobase = A in
        print_string " A = "; print_endline (nucleobase_to_string (rna_nucleobase_pairing base));

        let helll = generate_helix 4 in
        print_helix helll;
        print_rna (generate_rna helll);

        print_triplet_base (generate_bases_triplets (generate_rna helll))

