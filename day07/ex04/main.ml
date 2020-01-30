
let main () = 
	let gal = (new Galifrey.galifrey) in 
	let gal1 = (gal#creation_dalek 15) in
	let gal2 = (gal1#creation_people 15 ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "11"; "12"; "13"; "14"; "15"]) in
	let gal3 = (gal2#add_doctor) in

	gal3#do_time_war

let () = main ()

