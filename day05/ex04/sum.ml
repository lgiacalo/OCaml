(*

Seriously, just a sum. There’s no catch.
Turn-in directory : ex04/
Files to turn in : sum.ml
Allowed functions : None

You will write a function named sum which takes two floating-point numbers and adds
one to the other. Yes. That’s it.
Your function’s type will be float -> float -> float.

*)

let sum a b = a +. b

let () = 
	if (Array.length Sys.argv) > 2
	then
	begin try 
		let a = float_of_string (Array.get Sys.argv 1) and
			b = float_of_string (Array.get Sys.argv 2) in
		print_float a; print_string " +. "; print_float b;
		print_string " = "; print_float (sum a b); print_endline ""

	with | Failure err -> print_endline ("Error argument: " ^ err)
	end;

	print_string "42.42 + 42.42 = ";
	print_float (sum 42.42 42.42);
	print_endline ""