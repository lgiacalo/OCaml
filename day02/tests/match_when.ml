let inbound x = match x with
	| y when (y > 0 && y < 100) -> "True"
	| _ -> "False"

let () = 
	print_endline (inbound (-1));
	print_endline (inbound 50)