
(*


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
        | A -> "A" | T -> "T" | C -> "C" | G -> "G" | U -> "U" | _ -> "_"


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

let string_of_aminoacid (am:aminoacid) = match am with
        | Stop  -> "Stop"
        | Ala   -> "Alamine"
        | Arg   -> "Arginine"
        | Asn  -> "Asparagine"
        | Asp  -> "Aspartique"
        | Cys  -> "Cysteine"
        | Gln  -> "Glutamine"
        | Glu  -> "Glutamique"
        | Gly  -> "Glycine"
        | His  -> "Histidine"
        | Ile  -> "Isoleucine"
        | Leu  -> "Leucine"
        | Lys  -> "Lysine"
        | Met  -> "Methionine"
        | Phe  -> "Phenylalanine"
        | Pro  -> "Proline"
        | Ser  -> "Serine"
        | Thr  -> "Threomine"
        | Trp  -> "Tryptophane"
        | Tyr  -> "Tyrosine"
        | Val  -> "Valine"


let string_of_protein (prot:protein) = match prot with
        | [] -> ""
        | _ -> (
                let rec loop prot str = match prot with
                        | [] -> str
                        | first::last -> loop last (str ^ (string_of_aminoacid first))
                in
                loop prot ""
)


(********************************* FONCTIONS ******************************************)

let generate_nucleobase_random = match (Random.int 4) with
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


let decode_arn (r:rna) = match r with
        | [] -> []
        | _ -> (
                let triplet = generate_bases_triplets r in
                let rec loop trip lst = match trip with
                        | (U,A,A)::(U,A,G)::(U,G,A)::last -> (lst @ [Stop])
                        | (G,C,A)::(G,C,C)::(G,C,G)::(G,C,U)::last -> loop last (lst @ [Ala])
                        | (A,G,A)::(A,G,G)::(C,G,A)::(C,G,C)::(C,G,G)::(C,G,U)::last -> loop last (lst @ [Arg])
                        | (A,A,C)::(A,A,U)::last -> loop last (lst @ [Asn])
                        | (G,A,C)::(G,A,U)::last -> loop last (lst @ [Asp])
                        | (U,G,C)::(U,G,U)::last -> loop last (lst @ [Cys])
                        | (C,A,A)::(C,A,G)::last -> loop last (lst @ [Gln])
                        | (G,A,A)::(G,A,G)::last -> loop last (lst @ [Glu])
                        | (G,G,A)::(G,G,C)::(G,G,G)::(G,G,U)::last -> loop last (lst @ [Gly])
                        | (C,A,C)::(C,A,U)::last -> loop last (lst @ [His])
                        | (A,U,A)::(A,U,C)::(A,U,U)::last -> loop last (lst @ [Ile])
                        | (C,U,A)::(C,U,C)::(C,U,G)::(C,U,U)::(U,U,A)::(U,U,G)::last -> loop last (lst @ [Leu])
                        | (A,A,A)::(A,A,G)::last -> loop last (lst @ [Lys])
                        | (A,U,G)::last -> loop last (lst @ [Met])
                        | (U,U,C)::(U,U,U)::last -> loop last (lst @ [Phe])
                        | (C,C,C)::(C,C,A)::(C,C,G)::(C,C,U)::last -> loop last (lst @ [Pro])
                        | (U,C,A)::(U,C,C)::(U,C,G)::(U,C,U)::(A,G,U)::(A,G,C)::last -> loop last (lst @ [Ser])
                        | (A,C,A)::(A,C,C)::(A,C,G)::(A,C,U)::last -> loop last (lst @ [Thr])
                        | (U,G,G)::last -> loop last (lst @ [Trp])
                        | (U,A,C)::(U,A,U)::last -> loop last (lst @ [Tyr])
                        | (G,U,A)::(G,U,C)::(G,U,G)::(G,U,U)::last -> loop last (lst @ [Gly])
                        | _ -> lst
                in
                loop triplet []
)


(********************************* MAIN ******************************************)

let main () = 
        Random.self_init ();


        let base:nucleobase = A in
        print_string " A = "; print_endline (nucleobase_to_string (rna_nucleobase_pairing base));

        let helll = generate_helix 8 in
        print_helix helll;
        print_rna (generate_rna helll);

        print_triplet_base (generate_bases_triplets (generate_rna helll));

        print_string (string_of_protein (decode_arn (generate_rna helll)))

let () = main ()
