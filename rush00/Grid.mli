module Player :
sig
	
	type mark = X | O | Empty

	type t = {
		name: string;
		symb: mark
	}

	val	newPlayer : string -> mark -> t

	val displayWinGame 	: t -> unit
	val displayWinGrid 	: t -> int -> unit

	val displayTurn : t -> unit

	val getName : t -> string
	val getSymb : t -> mark

	val getSymbtoString : mark -> string
end

module Morpion :
sig

	type state = P of Player.t | None

	type m = { 
			state: state; 
			grid: Player.mark list
	}

	val newMorpion 		: unit -> m 					(*initialisation state = None; grid = [-;-;...] *)
	val newState   		: Player.t -> state
	val createMorpion : Player.mark list -> m

	val getState 	 : m -> state
	val getGrid 	 : m -> Player.mark list
	val getStateName : m -> string
	val getStateSymb  : m -> Player.mark

	val updateState	: m -> state -> m 	
	val updateGrid	: m -> Player.mark list -> m

	val displayLine 	: m -> int -> unit

	val isWon 		: state -> bool						(* return true if grid is ended, else run *)
	val isFull		: Player.mark list -> bool				(* si grille pleine *)
	val isFree		: Player.mark list -> int -> int -> bool	
	val putSymb		: Player.mark list -> Player.t -> int -> int -> Player.mark list

	val checkGrid	: Player.mark list -> bool

	val playTurn 	: m -> Player.t -> int -> int -> int -> m

end

type g = Morpion.m list

val newGrid : unit -> g

val updateGrid	: g -> Morpion.m -> int -> g

val	displayGrid : g -> unit

val isFull 	: g -> bool
val	isWon	: g -> bool
val	checkGrid	: g -> bool

val ia_move 	: Morpion.m -> Player.t -> int -> Morpion.m * int
val playTurn	: g -> Player.t -> bool -> g * bool
val playGame 	: Player.t -> Player.t -> bool -> unit

























