let ft_is_upper c = 
        c <= 'Z' && c >= 'A'
let ft_is_lower c = 
        c <= 'z' && c >= 'a'

let ft_rot_n n str = 
        let rot = (n mod 26) in
        let ft_rot c = 
                let ascii = int_of_char c in
                if (ft_is_upper c)
                then
                        if (ascii + rot > (int_of_char 'Z'))
                        then char_of_int ((int_of_char 'A') + ascii + rot - (int_of_char 'Z'))
                        else char_of_int (ascii + rot)
                else if (ft_is_lower c)
                then
                        if (ascii + rot > (int_of_char 'z'))
                        then char_of_int ((int_of_char 'a') + ascii + rot - (int_of_char 'z'))
                        else char_of_int (ascii + rot)
                else c
         in
         String.map ft_rot str


let main () = 
        print_string ((ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz") ^ "\n");
        print_string ((ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz") ^ "\n");
        print_string ((ft_rot_n 42 "0123456789") ^ "\n");
        print_string ((ft_rot_n 2 "OI2EAS67B9") ^ "\n");
        print_string ((ft_rot_n 0 "Damned !") ^ "\n");
        print_string ((ft_rot_n 42 "") ^ "\n");
        print_string ((ft_rot_n 1 "NBzlk qnbjr !") ^ "\n")

let () = main ()
