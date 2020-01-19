let ft_print_number nb = 
        if nb < 10
        then print_int 0;
        print_int nb


let ft_print_comb () =
        let first = 0 in
        let rec loop1 n1 = 
                if n1 <= 98
                then
                        let rec loop2 n2 = 
                                if n2 <= 99
                                then
                                        begin
                                        ft_print_number n1;
                                        print_char ' ';
                                        ft_print_number n2;
                                        if (n1 <> 98 || n2 <> 99)
                                        then 
                                                begin
                                                print_char ',';
                                                print_char ' ';
                                                end
                                        else
                                                print_char '\n';
                                        loop2 (n2 + 1)
                                        end;
                         in
                         loop2 (n1 + 1);
                         loop1 (n1 + 1)
        in
        loop1 first;
        print_char '\n'

let main () = 
        ft_print_comb ()

let () = main ()
