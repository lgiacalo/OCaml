let ft_sum f min max = 
        if max < min
        then nan
        else 
                let rec loop min sum = 
                if min = max
                then sum +. (f min)
                else
                        loop (min + 1) (sum +. (f min))
                in
                loop min 0.0

let main () = 
        print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
        print_char '\n';
        print_float (ft_sum (fun i -> float_of_int (i)) 1 10);
        print_char '\n';
        print_float (ft_sum (fun i -> float_of_int (i)) 1 0);
        print_char '\n';
        print_float (ft_sum (fun i -> float_of_int (i * i)) 2 2);
        print_char '\n'

let () = main ()
