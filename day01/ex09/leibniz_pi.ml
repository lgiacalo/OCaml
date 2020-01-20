let ft_fabs nb = 
        if (nb < 0.)
        then nb *. -1.
        else nb


let leibniz delta = 
        if delta < 0.
        then (-1)
        else
                let pi = (4. *. (atan 1.)) in
                let rec loop i =
                begin
                        print_float (4. *. (atan (float_of_int i)));
                        print_endline " -- ";
                        let res = (4. *. (atan (float_of_int i))) in
                        if ((ft_fabs (res -. pi)) < delta)
                        then i
                        else loop (i + 1)
                end
                in
                loop 0

let main () = 
        print_float (4. *. (atan 1.));
        print_string "\n\n";
        print_int (leibniz (-42.));
        print_string "\n\n\n";
        print_int (leibniz (1.));
        print_string "\n\n\n";
        print_int (leibniz (0.001));
        print_string "\n";
        print_char '\n'

let () = main ()
