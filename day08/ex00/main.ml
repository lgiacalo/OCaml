
let main () = 
	let hydrogen = new Atom.hydrogen in
	let helium = new Atom.helium in
	let lithium = new Atom.lithium in
	let carbon = new Atom.carbon in
	let azote = new Atom.azote in
	let oxygen = new Atom.oxygen in
	let fluor = new Atom.fluor in
	let sodium = new Atom.sodium in
	let fer = new Atom.fer in
	let cobalt = new Atom.cobalt in
	let nickel = new Atom.nickel in
	let cuivre = new Atom.cuivre in
	let zinc = new Atom.zinc in

	print_endline "To_string : ";
	print_endline hydrogen#to_string;
	print_endline helium#to_string;
	print_endline lithium#to_string;
	print_endline carbon#to_string;
	print_endline azote#to_string;
	print_endline oxygen#to_string;
	print_endline fluor#to_string;
	print_endline sodium#to_string;
	print_endline fer#to_string;
	print_endline cobalt#to_string;
	print_endline nickel#to_string;
	print_endline cuivre#to_string;
	print_endline zinc#to_string;

	print_endline "\nEquals : ";
	Printf.printf "%s = %s    ? %B\n" hydrogen#to_string fluor#to_string (hydrogen#equals fluor);
  	Printf.printf "%s = %s ? %B\n" cuivre#to_string cuivre#to_string (cuivre#equals cuivre);
  	Printf.printf "%s   = %s ? %B\n" oxygen#to_string nickel#to_string (oxygen#equals nickel);
  	Printf.printf "%s   = %s   ? %B\n" zinc#to_string zinc#to_string (zinc#equals zinc)

let () = main ()