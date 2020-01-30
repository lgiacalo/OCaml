(*      TODO: Retirer propane + butane     *)

let main () = 
	let water = new Molecule.water in
	print_endline water#to_string;

	let trinitrotoluene = new Molecule.trinitrotoluene in
	print_endline trinitrotoluene#to_string;

	let carbon_dioxyde = new Molecule.carbon_dioxyde in
	print_endline carbon_dioxyde#to_string;

	let glucose = new Molecule.glucose in
	print_endline glucose#to_string;

	let butane = new Molecule.butane in
	print_endline butane#to_string;

	let alanine = new Molecule.alanine in
	print_endline alanine#to_string;

	let ethanol = new Molecule.ethanol in
	print_endline ethanol#to_string;

	let propane = new Molecule.propane in
	print_endline propane#to_string;

	let sucre = new Molecule.sucre in
	print_endline sucre#to_string;


	()

let () = main ()


