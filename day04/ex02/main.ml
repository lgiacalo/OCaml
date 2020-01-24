


let main () = 
        print_endline "****** Values Cards ******";

        let (card:Card.t) = Card.newCard Card.Value.T2 Card.Color.Spade in
        print_endline ("Value = " ^ (Card.Value.toStringVerbose card.value) ^ " / Color = " ^ (Card.Color.toStringVerbose card.color));

        print_endline "**************************"


let () = main ()
