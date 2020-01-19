
let ackermann m n = 
        let rec loop m n acc = 
        if m < 0 || n < 0
        then -1
        else if m = 0
        then (n + 1)
        else if m > 0 && n = 0
        then (loop (m - 1) 1 (acc + 1))
        else (loop (m - 1) (loop m (n - 1) (acc + 1)) (acc + 1))
        in 
        loop m n 0


let main () = 
        print_int (ackermann (-1) 7);
        print_char '\n';
        print_int (ackermann 0 0);
        print_char '\n';
        print_int (ackermann 2 3);
        print_char '\n';
        print_int (ackermann 4 1);
        print_char '\n'

let () = main ()
