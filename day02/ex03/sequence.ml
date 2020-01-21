
let encode liste = match liste with
	| []	-> 	[]
	| _		->	
	begin
		let rec loop lst lstt acc = match lst with
			| first::second::last when first = second -> loop (second::last) lstt (acc + 1)
                        | first::last -> loop last (lstt @ [acc] @ [first]) 1
			| [] -> lstt
		in
		loop liste [] 1
	end

let string_of_lst lst = match lst with
        | [] -> ""
        | _ -> (
                let rec loop lst str = match lst with
                        | [] -> str
                        | first::last -> (
                                loop last (str ^ (string_of_int first))
                        )
                in 
                loop lst ""
        )

let sequence n = 
        if n <= 0
        then ""
        else if n = 1
        then "1"
        else
                let rec loop n lst = 
                let liste = (encode lst) in
                if n = 1
                then liste
                else
                        loop (n - 1) liste
                in
                string_of_lst (loop (n - 1) [1])




let () = 
        print_string "sequence (-42): ";
        print_endline (sequence (-42));
        print_string "sequence 0: ";
        print_endline (sequence 0);
        print_string "sequence 1: ";
        print_endline (sequence 1);
        print_string "sequence 2: ";
        print_endline (sequence 2);
        print_string "sequence 3: ";
        print_endline (sequence 3);
        print_string "sequence 4: ";
        print_endline (sequence 4);
        print_string "sequence 5: ";
        print_endline (sequence 5);




