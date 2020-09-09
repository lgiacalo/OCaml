

module Player =
struct

	type mark = X | O | Empty
	
	type t = {
		name: string;
		symb: mark
	}

	let	newPlayer n s = {name = n; symb = s}

	let displayWinGame t 	= print_endline (t.name ^ " win the game!")
	let displayWinGrid t n	= print_endline (t.name ^ " win grid " ^ (string_of_int (n + 1)) ^ "!")
	let displayTurn    t 	= print_endline (t.name ^ "'s turn to play.")

	let getName t = t.name
	let getSymb t = t.symb

	let getSymbtoString mark = match mark with
	| X -> "X"
	| O -> "O"
	| Empty -> "-"

end

module Morpion =
struct

	type state = P of Player.t | None

	type m = { 
			state: state; 
			grid: Player.mark list
	}

	let newMorpion () = {
		state = None;
		grid = [Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty]
	}

	let newState player = P player 

	let createMorpion grid = {
		state = None;
		grid = grid
	}
 	
	let getState m = m.state
	let getGrid m  = m.grid

	let getStateName m = match m.state with
		| None -> invalid_arg "No state name for None"
		| P player -> (Player.getName player)

	let getStateSymb m = match m.state with
		| None -> invalid_arg "No state name for None"
		| P player -> (Player.getSymb player)

	let updateState m state = {
		state = state;
		grid = m.grid
	}

	let updateGrid m grid = {
		state = m.state;
		grid = grid
	}

	let displayLineWin m n = match getStateSymb m with
		| O when (n = 0) -> print_string "/ - \\"
		| O when (n = 1) -> print_string ("| " ^ (" ") ^ " |")
		| O -> print_string "\\ - /" 
		| _ when (n = 0) -> print_string "\\   /"
		| _ when (n = 1) -> print_string ("  " ^ ("X") ^ "  ")
		| _ -> print_string "/   \\" 

	let displayLine m n = match (getState m) with
		| None -> (
				print_string (Player.getSymbtoString (List.nth (getGrid m) (n * 3)));
				print_string (" " ^ (Player.getSymbtoString (List.nth (getGrid m) ((n * 3) + 1))));
				print_string (" " ^ (Player.getSymbtoString (List.nth (getGrid m) ((n * 3) + 2))))
			)
		| _ -> displayLineWin m n


	let isWon state = match state with
		| None -> false
		| _ -> true

	let rec isFull grid = match grid with
		| [] -> true
		| c::last when (c = Player.Empty) -> false
		| c::last -> (isFull last)

	let isFree grid l c =
		let nb = ((3 * l) + c) in
		(List.nth grid nb) = Player.Empty	

	let putSymb grid player l c = 
		let nb = ((3 * l) + c) in
		let rec loop grid n llst = match grid with
			| [] -> llst
			| c::last when (n = nb) -> (llst @ [(Player.getSymb player)] @ last)
			| c::last -> loop last (n + 1) (llst @ [c])
		in
		loop grid 0 []


	let rec checkLine grid = match grid with
		| [] -> false
		| a::b::c::last when (a = b) && (b = c) && (c <> Player.Empty) -> true
		| a::b::c::last -> (checkLine last) 
		| _ -> invalid_arg "Invalid Morpion"

 	let checkCol grid = 
		let rec loop n = match n with
			| 3 -> false
			| x when ((List.nth grid n) = (List.nth grid (n + 3))) 
					&& (List.nth grid (n + 3) = (List.nth grid (n + 6))) 
					&& ((List.nth grid (n + 3) <> Player.Empty)) -> true
			| _ -> loop (n + 1)
		in
		loop 0
 
 	let checkDiag grid = 
 		let d1 = ((List.nth grid 0) = (List.nth grid 4)) && ((List.nth grid 4) = (List.nth grid 8)) && ((List.nth grid 4) <> Player.Empty) in
 		let d2 = ((List.nth grid 2) = (List.nth grid 4)) && ((List.nth grid 4) = (List.nth grid 6)) && ((List.nth grid 4) <> Player.Empty) in
 		d1 || d2

	let checkGrid grid = 
		(checkLine grid) || (checkCol grid) || (checkDiag grid)

	let playTurn m player l c n = 
		let new_m = (updateGrid m (putSymb m.grid player l c)) in
		if ((checkGrid new_m.grid) || (isFull new_m.grid)) 
		then
			begin
			Player.displayWinGrid player n;
			updateState new_m (newState player)
			end
		else new_m
	
	let find_moves gg isMaxPlayer =
		let s = begin
			if isMaxPlayer then Player.X
			else Player.O
		end in
		let rec loop g h l = match g with
			| [] -> l
			| hd::t when (hd = Player.Empty) -> loop t (h @ [hd]) (l @ [(h @ [s] @ t)])
			| hd::t -> loop t (h @ [hd]) l

		in
		loop gg [] []

	let rec minmax g isMaxPlayer =
		if ((checkGrid g) || (isFull g))
		then
			begin
				if isMaxPlayer then (-1)
				else 1
			end
		else
			if isMaxPlayer
			then
				begin
					let bestVal = (-42) in
					let moves = find_moves g isMaxPlayer in
					let rec loop_max llst b_v = match llst with
						| [] -> b_v
						| h::t -> loop_max t (max (minmax h false) b_v)
					in loop_max moves bestVal
				end
			else
				begin
					let bestVal = 42 in
					let moves = find_moves g isMaxPlayer in
					let rec loop_min llst b_v = match llst with
						| [] -> b_v
						| h::t -> loop_min t (min (minmax h true) b_v)
					in loop_min moves bestVal
				end							

end

type g = Morpion.m list

let newGrid	() = 
	let rec loop n g = match n with
		| 0 -> g
		| _ -> loop (n - 1) (g @ [Morpion.newMorpion ()])
	in loop 9 []

let updateGrid g m n = 
	let rec loop g i new_g = match g with
		| [] -> new_g
		| grid::last when (i = n) -> (new_g @ [m] @ last)
		| grid::last -> loop last (i + 1) (new_g @ [grid])
	in loop g 0 []


let rec isFull g = match g with
	| [] -> true
	| grid::last when (Morpion.isFull (Morpion.getGrid grid)) -> (isFull last)
	| grid::last -> false


let rec isWon g = match g with
	| [] -> true
	| grid::last when (Morpion.isWon (Morpion.getState grid)) -> (isWon last)
	| grid::last -> false



let isWinnerEqual a b c =
	if ((Morpion.getState a = None) || (Morpion.getState b = None) || (Morpion.getState c = None)) then false
	else
		begin
			let nameA = (Morpion.getStateName a) in
			let nameB = (Morpion.getStateName b) in
			let nameC = (Morpion.getStateName c) in
			(nameA = nameB) && (nameB = nameC)
		end

let rec checkLine g = match g with
	| [] -> false
	| a::b::c::last when (isWinnerEqual a b c) -> true
	| a::b::c::last -> checkLine last
	| _ -> invalid_arg "Invalid Grid"

let checkCol g = 
	let rec loop n = match n with
		| 3 -> false
		| x when (isWinnerEqual (List.nth g n) (List.nth g (n + 3)) (List.nth g (n + 6))) -> true
		| _ -> loop (n + 1)
	in
	loop 0

let checkDiag g = 
		let d1 = (isWinnerEqual (List.nth g 0) (List.nth g 4) (List.nth g 8)) in
		let d2 = (isWinnerEqual (List.nth g 2) (List.nth g 4) (List.nth g 6)) in
 		d1 || d2

let	checkGrid g = 
	(checkLine g) || (checkCol g) || (checkDiag g) 


let displayBlock a b c = 
	let rec loop n = match n with
		| 3 -> ()
		| _ -> (Morpion.displayLine a n;
				print_string " | ";
				Morpion.displayLine b n;
				print_string " | ";
				Morpion.displayLine c n;
				print_endline "";
				loop (n + 1)
		)
	in loop 0

	
let displayGrid g =
	let rec display gg = match gg with
		| [] -> ()
		| a::b::c::last -> (
			displayBlock a b c;
			if (last <> [])
			then print_endline "---------------------";
			display last
		)
		| _ -> invalid_arg "Invalid Grid"
	in
	print_endline "";
	display g;
	print_endline ""

let read_input () =
	let input = read_line () in
	let lst = ( String.split_on_char ' ' (String.trim input) ) in
	let rec loop l llst = match l with
		| hd::tl when (hd = "") -> loop tl llst
		| hd::tl -> loop tl (llst @ [(String.trim hd)])
		| [] -> llst
	in loop lst []

let is_digit c = 
        c >= '0' && c <= '9'

let check_string f ss =
	let s = (String.trim ss) in
	let len = String.length s in
	let rec loop n = match n with
		| 0 -> true
		| _ when is_digit (String.get s (n - 1)) -> loop (n - 1)
		| _ -> false
	in loop len

let check_input lst =
	if List.length lst <> 2 then (print_endline "Incorrect format." ; false)
	else if (check_string (is_digit) (List.nth lst 0)) && (check_string (is_digit) (List.nth lst 1))
	then 
		begin
			let l = int_of_string (String.trim (List.nth lst 0)) in 
			let c = int_of_string (String.trim (List.nth lst 1)) in
				if (l < 10 && c < 10) && (l > 0 && c > 0) then true
				else (print_endline "Illegal move." ; false)
		end
	else (print_endline "Incorrect format." ; false)

let convert_move lst = 
	let l = int_of_string (List.nth lst 0) in
	let c = int_of_string (List.nth lst 1) in
	((((l - 1) / 3 ) * 3 + ((c - 1) / 3)), ((l - 1) mod 3), ((c - 1) mod 3))

let ia_move m player n =
	let moves = Morpion.find_moves (Morpion.getGrid m) true in
	let rec loop move c = match move with
		| [] -> c
		| h::t -> 
			begin
				let nb = (Morpion.minmax h false) in
				let (m, score) = c in
				if (nb > score) then loop t (h, nb)
				else loop t c
			end
	in 
	let (new_g, i) = loop moves ((List.nth moves 0), (-42)) in
	let new_m = Morpion.createMorpion new_g in
	if ((Morpion.checkGrid new_g) || (Morpion.isFull new_g)) 
	then
		begin
			Player.displayWinGrid player n;
			((Morpion.updateState new_m (Morpion.newState player)), i)
		end
	else (new_m, i)

let rec playTurn g player ia =
	if ia = false then
		begin
			let input = read_input () in
			if ((check_input input) = false) then (playTurn g player ia)
			else
				begin
					let (n, l, c) = convert_move input in
					if ((Morpion.isFree (Morpion.getGrid (List.nth g n)) l c = false)
						|| (Morpion.isWon (Morpion.getState (List.nth g n))))
					then (print_endline "Illegal move." ; playTurn g player ia)
					else
						begin
							let g_new = updateGrid g (Morpion.playTurn (List.nth g n) player l c n) n in
							if ((checkGrid g_new) || (isFull g_new) || (isWon g_new))
							then ((Player.displayWinGame player) ; displayGrid g_new ; (g_new, true))
							else (displayGrid g_new ; (g_new, false)) 
						end
				end
		end
	else
	begin
		let rec loop n ret = 
			let (m, score, nb) = ret in
			match n with
			| 9 -> (m, nb)
			| _ when ((Morpion.getState (List.nth g n)) = None) -> (
				let (m, score) = (ia_move (List.nth g n) player n) in
				if (score < 0) then loop (n + 1) (m, score, n)
				else (m, n)
			)
			| _ -> loop (n + 1) ret
		in
		let (m, n) = loop 0 ((Morpion.newMorpion ()), 0, 0) in
		let g_new = updateGrid g m n in
		if ((checkGrid g_new) || (isFull g_new) || (isWon g_new))
		then ((Player.displayWinGame player) ; displayGrid g_new ; (g_new, true))
		else (displayGrid g_new ; (g_new, false)) 
	end


let check_input_newGame input = 
	if (List.length input <> 1) then false
	else (
		let hd = (List.nth input 0) in
		if (String.length hd = 1) && ((String.get hd 0) = 'Y')
		then true
		else false
	)

let rec playGame p1 p2 ia =
	let g = newGrid () in
	displayGrid g ;
	let rec oneTurn g =
		Player.displayTurn p1;
		let (g_1, b1) = playTurn g p1 false in
			if b1 then print_endline ""
			else
				begin
					Player.displayTurn p2;
					let (g_2, b2) = playTurn g_1 p2 ia in
					if b2 then print_endline ""
					else oneTurn g_2 
				end
	in oneTurn g; 
	print_endline "New game ? (Y/N)";
	let input = read_input () in
	if (check_input_newGame input) then playGame p1 p2 ia


