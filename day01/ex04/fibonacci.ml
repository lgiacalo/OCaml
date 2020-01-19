let rec fibonacci n =
        if n < 0
        then -1
        else if n <= 1
        then n
        else
                (fibonacci (n - 2)) + (fibonacci (n - 1)) 


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
