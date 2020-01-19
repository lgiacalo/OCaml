let rec converges f x n = 
        if (n < 0)
        then 
                false
        else
                if (f x) = x
                then
                        true
                else
                        (converges f (f x) (n - 1))

let answer b =
        if b
        then  print_string "true\n"
        else  print_string "false\n"

let main () = 
        answer (converges (( * ) 2) 2 5);
        answer (converges (fun x -> x / 2) 2 3);
        answer (converges (fun x -> x / 2) 2 2)

let () = main ()


