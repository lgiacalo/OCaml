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
               if func1 c
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
        then  print_string "true\n"
        else  print_string "false\n"

let ft_is_palindrome str = 
        let min = 0 in
        let max = String.length str in
        if max <> 0
        then
                let rec loop one two = 
                        if one <= two
                        then if (String.get str one) = (String.get str (two - 1))
                             then
                                     true && (loop (one + 1) (two - 1))
                             else
                                     false
                        else 
                                true
                in
                loop min max


        else
                true



        
let main () = 
        answer (ft_is_palindrome "radar");
        answer (ft_is_palindrome "madam");
        answer (ft_is_palindrome "car");
        answer (ft_is_palindrome "");
        print_char '\n'

let () = main ()
