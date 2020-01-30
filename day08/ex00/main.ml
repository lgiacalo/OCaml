
let main () = 
	let hydrogen = new Atom.hydrogen in
	let helium = new Atom.helium in
	let lithium = new Atom.lithium in
	let carbon = new Atom.carbon in
	let nitrogen = new Atom.nitrogen in
	let oxygen = new Atom.oxygen in
	let fluorine = new Atom.fluorine in
	let sodium = new Atom.sodium in
	let iron = new Atom.iron in
	let cobalt = new Atom.cobalt in
	let nickel = new Atom.nickel in
	let copper = new Atom.copper in
	let zinc = new Atom.zinc in

	print_endline "To_string : ";
	print_endline (hydrogen#to_string ^ hydrogen#name);
	print_endline helium#to_string;
	print_endline (lithium#to_string ^ lithium#symbol);
	print_endline carbon#to_string;
	print_endline nitrogen#to_string;
	print_endline oxygen#to_string;
	print_endline fluorine#to_string;
	print_endline (sodium#to_string ^ (string_of_int sodium#atomic_number)); 
	print_endline iron#to_string;
	print_endline cobalt#to_string;
	print_endline nickel#to_string;
	print_endline copper#to_string;
	print_endline zinc#to_string;

	print_endline "\nEquals : ";
	Printf.printf "%s = %s    ? %B\n" hydrogen#to_string fluorine#to_string (hydrogen#equals fluorine);
  	Printf.printf "%s = %s ? %B\n" copper#to_string copper#to_string (copper#equals copper);
  	Printf.printf "%s   = %s ? %B\n" oxygen#to_string nickel#to_string (oxygen#equals nickel);
  	Printf.printf "%s   = %s   ? %B\n" zinc#to_string zinc#to_string (zinc#equals zinc)

let () = main ()