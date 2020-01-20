let leibniz_pi delta =
	let pi = float_of_int 4 *. (atan 1.) in
	let fabs x =
		if x < 0. then
			(x *. -1.)
		else
			x
	in
	let diff x =
		if fabs (pi -. x) < delta
		then true
		else false
	in
	let sigma acc i =
		acc +. ((-1. ** float_of_int i) /. float_of_int(2 * i + 1))
	in
	let rec compute i acc =
		if diff ((sigma acc i) *. 4.) then
			i
		else
			compute (i + 1) (sigma acc i)
	in
	if delta < 0.
		then (-1)
	else
		compute 0 0.


let () =
	print_endline "testing delta = -42.: ";
   print_int (leibniz_pi (-42.));
   print_endline "\ntesting delta = 0.1: ";
   print_int (leibniz_pi 0.1);
   print_endline "\ntesting delta = 0.01: ";
   print_int (leibniz_pi 0.01);
   print_endline "\ntesting delta = 0.001: ";
   print_int (leibniz_pi 0.001);
   print_endline ""
