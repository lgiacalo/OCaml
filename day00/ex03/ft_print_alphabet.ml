let ft_print_alphabet () = 
        let aa = int_of_char 'a' in
        let zz = int_of_char 'z' in
        let rec loop ascii = 
                if (ascii <= zz)
                then 
                  begin
                        print_char (char_of_int ascii);
                        loop (ascii + 1)
                  end;
        in
        loop aa;
        print_char '\n'

let main () = 
        ft_print_alphabet ();

let () = main ()
