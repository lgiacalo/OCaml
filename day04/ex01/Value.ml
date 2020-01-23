(*


*)


type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

let toInt v = match v with
        | T2 -> 1 | T3 -> 2 | T4 -> 3 | T5 -> 4 | T6 -> 5 | T7 -> 6 | T8 -> 7 
        | T9 -> 8 | T10 -> 9 | Jack -> 10 | Queen -> 11 | King -> 12 | As -> 13

let toString v = match v with
        | T2 -> "2" | T3 -> "3" | T4 -> "4" | T5 -> "5" | T6 -> "6" | T7 -> "7" | T8 -> "8"
        | T9 -> "9" | T10 -> "10" | Jack -> "J" | Queen -> "Q" | King -> "K" | As -> "A"

let toStringVerbose v = match v with
        | Jack -> "Jack" | Queen -> "Queen" | King -> "King" | As -> "As"
        | _ -> toString v
        
let next v = 
        let rec loop lst = match lst with
                | [] -> invalid_arg "Invalid argument !"
                | one::two::last when (one = v) -> two
                | one::last -> loop last
        in 
        loop all

let prev v = 
        let rec loop lst pred = match lst with
                | [] -> invalid_arg "Invalid argument"
                | one::last -> (
                        if (one = v && pred <> T2) then pred
                        else loop last one
                ) 
        in 
        loop all T2




