

let game ?(n1="O") ?(n2="X") () =
	if ((n1 = n2) || (n1 = "") || (n2 = ""))
	then print_endline "Wrong name input."
	else (
		let p1 = Grid.Player.newPlayer n1 O in
		let p2 = Grid.Player.newPlayer n2 X in
		Grid.playGame p1 p2 (n2 = "IA")
	)

let main argc argv = match argc with
	| 1 -> game ()
	| 3 -> game ~n1:(String.trim (List.nth argv 1)) ~n2:(String.trim (List.nth argv 2)) ()
	| _ -> print_endline "Usage: ./tic-tac-toe [joueur1] [joueur2/IA]"

let () = 
	let argv = Array.to_list Sys.argv in
	main (List.length argv) argv