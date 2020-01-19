let fibonacci n =
        if n < 0
                then (-1)
        else
                let rec loop n x y = 
                        if n = 0
                        then x
                        else
                                loop (n - 1) y (x + y)
                in
                loop n 0 1


let main () = 
        print_int (fibonacci (-42));
        print_char '\n';
        print_int (fibonacci 1);
        print_char '\n';
        print_int (fibonacci 3);
        print_char '\n';
        print_int (fibonacci 6);
        print_char '\n'

let () = main ()
