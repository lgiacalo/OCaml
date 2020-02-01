module type App =
sig
	type project = string * string * int

	val zero : project
	val combine : project -> project -> project
	val fail : project -> project
	val success : project -> project
end

module App =
struct
	type project = string * string * int

	let zero = ("", "", 0)

	let fail p 		= let (s1, _, _) = p in (s1, "failed", 0)
	let success p 	= let (s1, _, _) = p in (s1, "succeed", 80)

	let combine p1 p2 = let (sa1, _, n1) = p1 and (sa2, _, n2) = p2 in  
						let m = ((n1 + n2) / 2) in 
						let st = if (m > 80) then "succeed" else "failed" in
						((sa1 ^ sa2), st, m)
end


let print_proj p = let (s1, st, n) = p in 
		print_endline ("Project(" ^ s1 ^ ", " ^ st ^ ", " ^ (string_of_int n) ^ ")")

let () = 

	let bad = ("bad", "failed", 0) in
    let good = ("good", "succeed", 90) in

    print_proj bad;
    print_proj good;

    print_proj (App.fail good);
    print_proj (App.success bad);

    print_proj (App.combine bad good);
    print_proj (App.combine good good)



