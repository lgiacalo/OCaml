(*
 *
Le tout début de l'adn se déroule dans une structure constituée d'un acide phosphate lié à un désoxyribose, lui-même lié à une nucléobase. 
Une liste de nombreuses structures s'appelle une hélice et deux d'entre elles constituent un échantillon d'ADN.
Hélice: P - D - Base, P - D - Base, ...

- Créez le type phosphate qui est un alias pour le type de chaîne.
- Créez le type désoxyribose qui est également un alias pour le type de chaîne.
- Créez la nucléobase de type variant. Ses constructeurs sont A, T, C, G et None.

- Écrivez le type de nucléotide qui contient 3 éléments: un phosphate, un désoxyribose et une nucléobase.
Le type de type de nucléotide est à vous, un enregistrement ou un tuple fera l'affaire.

- Ecrire une fonction generate_nucleotide qui retourne un nucléotide d'une nucléobase donnée passée en char. 
La fonction doit être typée comme :
        val generate_nucleotide: char -> nucleotide
Réglez la valeur de phosphate sur "phosphate" et la valeur de désoxyribose sur "désoxyribose".

 *)

type phosphate          = string
type deoxyribose        = string
type nucleobase         = A | T | C | G | None

type nucleotide         = {
        p       : phosphate;
        d       : deoxyribose;
        base    : nucleobase
}

let generate_nucleotide c = 
        {
                p       = "phosphate";
                d       = "deoxyribose";
                base    = match c with
                        | 'A' -> A
                        | 'T' -> T
                        | 'C' -> C
                        | 'G' -> G
                        | _ -> None
}

let print_nucleobase base = match base with
        | A -> print_endline "nucleobase  : A"
        | T -> print_endline "nucleobase  : T"
        | C -> print_endline "nucleobase  : C"
        | G -> print_endline "nucleobase  : G"
        | _ -> print_endline "nucleobase  : None"

let print_nucleotide nucl = 
        print_string "phosphate   : "; print_endline nucl.p;
        print_string "deoxyribose : "; print_endline nucl.d;
        print_nucleobase nucl.base;
        print_char '\n'

let () = 
        print_nucleotide (generate_nucleotide 'A');
        print_nucleotide (generate_nucleotide 'T');
        print_nucleotide (generate_nucleotide 'C');
        print_nucleotide (generate_nucleotide 'G');
        print_nucleotide (generate_nucleotide 'B')

        








