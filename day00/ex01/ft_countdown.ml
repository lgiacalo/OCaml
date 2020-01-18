let positive n = 
        if (n >= 0)
        then n
        else 0

let rec ft_countdown n = 
        begin
           print_int (positive n);
           print_char '\n'
        end;
        if (n > 0)
        then ft_countdown (n - 1)


let main () = 
        ft_countdown 3;
        ft_countdown 0;
        ft_countdown (-1)

let () = main ()
