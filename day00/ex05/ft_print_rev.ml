let ft_print_rev str = 
        let len = String.length str in
        let rec loop nb = 
                if nb <> 0
                then
                begin
                        print_char (String.get str (nb - 1));
                        loop (nb - 1)
                end;
        in
        loop len;
        print_char '\n'

let main () = 
        ft_print_rev "Hello world !";
        ft_print_rev ""

let () = main ()
