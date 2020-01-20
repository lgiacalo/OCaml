
let ft_fabs nb = 
        if (nb < 0.)
        then nb *. -1.
        else nb


let leibniz_pi delta = 
        if delta < 0.
        then (-1)
        else
        begin
                let pi = (4. *. (atan 1.)) in
                let num = fun i -> ((-1.) ** (float_of_int i)) in
                let den = fun i -> float_of_int(2 * i + 1) in
                let equation = fun i -> ((num i) /. (den i)) in
                let rec loop i acc = 
                        let tt = acc +. (equation i) in
                        if ((ft_fabs ((4. *. tt) -. pi)) < delta)
                        then i
                        else loop (i + 1) (tt)
                in
                loop 0 0.
        end

let main () = 
        print_endline "testing delta = -42.: ";
   print_int (leibniz_pi (-42.));
   print_endline "\ntesting delta = 0.1: ";
   print_int (leibniz_pi 0.1);
   print_endline "\ntesting delta = 0.01: ";
   print_int (leibniz_pi 0.01);
   print_endline "\ntesting delta = 0.001: ";
   print_int (leibniz_pi 0.001);
   print_endline "\ntesting delta = 0.0001: ";
   print_int (leibniz_pi 0.0001);
   print_endline "\ntesting delta = 0.00001: ";
   print_int (leibniz_pi 0.00001);
   print_endline "\ntesting delta = 0.000001: ";
   print_int (leibniz_pi 0.000001);
   print_endline "\ntesting delta = 0.0000001: ";
   print_int (leibniz_pi 0.0000001);
   print_endline "\ntesting delta = 0.00000001: ";
   print_int (leibniz_pi 0.00000001);
   print_endline ""

let () = main ()
