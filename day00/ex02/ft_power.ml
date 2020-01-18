let rec ft_power nb p = 
        if (p = 0)
        then 1
        else nb * (ft_power nb (p - 1))

let main () = 
        print_int (ft_power 2 4); print_char '\n';
        print_int (ft_power 2 3); print_char '\n';
        print_int (ft_power 3 0); print_char '\n';
        print_int (ft_power 0 5); print_char '\n'

let () = main ()
