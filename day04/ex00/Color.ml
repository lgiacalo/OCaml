(*


 *)

type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]

let toString cc = match cc with
        | Spade -> "S"
        | Heart -> "H"
        | Diamond -> "D"
        | Club -> "C"
       
let toStringVerbose cc = match cc with
        | Spade -> "Spade"
        | Heart -> "Heart"
        | Diamond -> "Diamond"
        | Club -> "Club"
