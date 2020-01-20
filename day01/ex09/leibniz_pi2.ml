let ft_sum f min max = 
        if max < min
        then nan
        else 
                let rec loop min sum = 
                if min = max
                then sum +. (f min)
                else
                        loop (min + 1) (sum +. (f min))
                in
                loop min 0.0

let ft_power nb p = 
        let rec loop p acc = 
                if (p = 0)
                then 1
                else (loop (p - 1) (acc * nb))
        in
        loop p 0


let rec ft_power_2 nb p = 
        if (p = 0)
        then 1
        else nb * (ft_power nb (p - 1))

let ft_fabs nb = 
        if (nb < 0.)
        then nb *. -1.
        else nb


let leibniz delta = 
        if delta < 0.
        then (-1)
        else
        begin
                print_float delta;
                print_endline " -- ";
                let pi = (4. *. (atan 1.)) in
                let num = fun i -> float_of_int(ft_power_2 (-1) i) in
                let den = fun i -> float_of_int(2 * i + 1) in
                let equation = fun i -> ((num i) /. (den i)) in
                let func = fun i -> (4. *. (ft_sum equation 0 i)) in
                let rec loop i = 
                begin
                        print_int i;
                        print_string " --- ";
                        print_float (equation i);
                        print_string " --- ";
                        print_float (ft_sum equation 0 i);
                        print_string " --- ";
                        print_float (func i);
                        print_string " --- ";
                        print_float (((ft_fabs ((func i) -. pi))));
                        print_endline " --- ";
                        if ((ft_fabs ((func i) -. pi)) < delta)
                        then i
                        else loop (i + 1)
                end
                in
                loop 0
        end

let main () = 
        print_float (4. *. (atan 1.));
        print_string "\n";
        print_int (leibniz (-42.));
        print_string "\n";
        print_int (leibniz (1.));
        print_string "\n";
        print_int (leibniz (0.1));
        print_string "\n";
        print_int (leibniz (0.01));
        print_char '\n'

let () = main ()
