
let print_int_base2 nbb pad = 
    let rec loop nb n = match n with
        | 0 -> ()
        | _ -> loop (nb / 2) (n - 1); print_int (nb mod 2)
    in
    loop nbb pad;
    print_char ' '

let grey n = 
    let rec loop i = match i with
        | i when i = int_of_float (2. ** (float_of_int n)) -> print_char '\n'
        | _ -> print_int_base2 (i lxor (i lsr 1)) n; loop (i + 1)
    in
    loop 0


let main () = 
        print_endline "start";

        print_int_base2 1 4;
        print_int_base2 2 4;
        print_int_base2 3 4;
        print_char '\n';

        print_endline "\ntest gray 3: ";
        grey 3;
        print_endline "\ntest gray 4: ";
        grey 4;

	print_endline "fin"

let () = main ()

