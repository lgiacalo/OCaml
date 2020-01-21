
(*

Un ARN messager est une molécule impliquée dans le processus de synthèse des protéines.
Le principal objectif de l'ARN est de créer une copie de travail complémentaire d'une hélice d'ADN.
C'est vraiment intelligent car il empêche l'ADN d'être altéré et permet plusieurs copies afin que le processus puisse être vraiment rapide.
Il a été introduit pour la première fois par les scientifiques Jacques Monod et François Jacob.

- Ecrire un type rna comme une liste d'éléments de type nucléobase.

- Ecrire une fonction generate_rna qui crée un élément de type rna à partir d'un élément de type helix selon les règles suivantes:

- Lors de la création, l'ARN est comme une hélice complémentaire sauf que la nucléobase T est commutée en nucléobase U (uracile). 
Modifiez votre type de nucléobase en conséquence. 
(Je vous ai dit de lire tout le sujet avant de commencer ...)

- La liste des nucléobases de l'ARN est la liste des nucléobases complémentaires de la nucléobase de l'hélice d'origine (à l'exception de la première règle).

Par exemple, la séquence de nucléobase "ATCGA" produira un [U; A; G; C; U]
 
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

(********************************* PRINTS *********************************************)

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


(********************************* MAIN ******************************************)

let () = 

        print_endline (helix_to_string (generate_helix 2));

        let hel = generate_helix 3 in
        print_helix hel;
        print_helix (complementary_helix hel);

        let hell = generate_helix 2 in
        print_helix hell;
        print_helix (complementary_helix hell);


        let base:nucleobase = A in
        print_string " A = "; print_endline (nucleobase_to_string (rna_nucleobase_pairing base));


        let helll = generate_helix 3 in
        print_helix helll;
        print_rna (generate_rna helll)

        

