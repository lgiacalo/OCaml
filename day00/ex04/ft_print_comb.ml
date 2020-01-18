let ft_print_comb () =
        let first = 0 in
        let rec loop1 n1 = 
                if n1 <= 7
                then
                        let rec loop2 n2 = 
                                if n2 <= 8
                                then
                                        let rec loop3 n3 = 
                                                if n3 <= 9
                                                then
                                                begin
                                                 print_int n1;
                                                 print_int n2;
                                                 print_int n3;
                                                 print_string ", ";
                                                 loop3 (n3 + 1)
                                                end;
                                        in
                                        loop3 (n2 + 1);
                                        loop2 (n2 + 1)
                         in
                         loop2 (n1 + 1);
                         loop1 (n1 + 1)
        in
        loop1 first;
        print_char '\n'

let main () = 
        ft_print_comb ()

let () = main ()
