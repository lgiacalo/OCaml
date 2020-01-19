
let rec repeat_string ?str:(s="x") x = 
        if x < 0
        then "Error"
        else if x = 0
        then ""
        else (s ^ (repeat_string ~str:s (x - 1)))

let main () = 
        print_endline (repeat_string (-1));
        print_endline (repeat_string 0);
        print_endline (repeat_string ~str:"Toto" 1);
        print_endline (repeat_string 2);
        print_endline (repeat_string ~str:"a" 5);
        print_endline (repeat_string ~str:"what" 3)

let () = main ()
