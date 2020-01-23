


let main () = 
        print_endline "****** Values Cards ******";
        let rec loop cc = match cc with
                | [] -> print_char '\n'
                | c::last -> (
                        print_string ((Value.toString c) ^ ": " ^ (Value.toStringVerbose c) ^ " - int ");
                        print_int (Value.toInt c);
                        print_char '\n';
                        loop last
        )
        in
        loop Value.all;

        print_endline ("next T5 = " ^ (Value.toStringVerbose (Value.next T5)));
        print_endline ("prev As = " ^ (Value.toStringVerbose (Value.prev As)));

        print_endline ("prev T2 = " ^ (Value.toStringVerbose (Value.prev T2)));
        print_endline ("next As = " ^ (Value.toStringVerbose (Value.next As)));

        print_endline "**************************"


let () = main ()
