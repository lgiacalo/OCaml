
let main () = 
	let meth = new Alkane.alkane 1 in print_endline meth#to_string;

	let methane = new Alkane.methane in print_endline methane#to_string;
	let ethane = new Alkane.ethane in print_endline ethane#to_string;
	let propane = new Alkane.propane in print_endline propane#to_string;
	let butane = new Alkane.butane in print_endline butane#to_string;
	let pentane = new Alkane.pentane in print_endline pentane#to_string;
	let hexane = new Alkane.hexane in print_endline hexane#to_string;
	let heptane = new Alkane.heptane in print_endline heptane#to_string;
	let octane = new Alkane.octane in print_endline octane#to_string;
	let nonane = new Alkane.alkane 9 in print_endline nonane#to_string;
	let decane = new Alkane.alkane 10 in print_endline decane#to_string;
	let undecane = new Alkane.alkane 11 in print_endline undecane#to_string;
	let dodecane = new Alkane.alkane 12 in print_endline dodecane#to_string;


	print_endline "\nEquals : ";
	Printf.printf "%s   = %s  ? %B\n" propane#to_string ethane#to_string (propane#equals ethane);
  	Printf.printf "%s    = %s  ? %B\n" methane#to_string meth#to_string (methane#equals meth);
  	Printf.printf "%s  = %s? %B\n" decane#to_string decane#to_string (decane#equals decane);
  	Printf.printf "%s= %s? %B\n" dodecane#to_string heptane#to_string (dodecane#equals heptane);

	()

let () = main ()

