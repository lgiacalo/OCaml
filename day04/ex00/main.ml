
let main () = 
        print_endline "****** Color ******";
        let rec loop cc = match cc with
                | [] -> print_char '\n'
                | c::last -> (
                        print_endline ((Color.toString c) ^ ": " ^ (Color.toStringVerbose c));
                        loop last
        )
        in
        loop Color.all

let () = main ()
