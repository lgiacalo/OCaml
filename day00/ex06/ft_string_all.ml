let is_digit c = 
        c >= '0' && c <= '9'

let ft_string_all func1 str = 
        let len = String.length str in
        if len > 0
        then
        let i = 0 in
        let rec loop nb =
           if nb >= len
           then true
           else
               let c = String.get str nb in
               if is_digit c
               then
                       true && (loop (nb + 1)) 
               else
                       false
        in
        loop (i)
        else
                false

let answer b =
        if b
        then 
                print_string "true\n"
        else
                print_string "false\n"

        
let main () = 
        answer (ft_string_all is_digit "0123456789");
        answer (ft_string_all is_digit "012EAS67B9");
        answer (ft_string_all is_digit "");
        print_char '\n'

let () = main ()
