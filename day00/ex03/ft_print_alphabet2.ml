let ft_print_alphabet2 () = 
        let rec loop ascii = 
                if ascii <= (int_of_char 'z')
                then 
                begin
                        print_char (char_of_int ascii);
                        loop (ascii + 1)
                end;
        in
        loop (int_of_char 'a');
        print_char '\n'


let main () = 
        ft_print_alphabet2 ()

let () = main ()

