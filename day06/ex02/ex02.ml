
module type VAL = 
sig 
	val x : int 
end

module type PAIR = 
sig 
	val pair : (int * int) 
end

(* FIX ME !!! *)

module type MAKEFST = 
	functor (IntPair : PAIR) -> VAL

module MakeFst : MAKEFST = 
	functor (IntPair : PAIR) -> 
	struct
		let x = (fst IntPair.pair)
	end 

module type MAKESND = 
	functor (IntPair : PAIR) -> VAL

module MakeSnd : MAKESND = 
	functor (IntPair : PAIR) -> 
	struct
		let x = (snd IntPair.pair)
	end 

(* FIX ME !!! *)

module Pair : PAIR = 
struct 
	let pair = ( 21, 42 ) 
end


module Fst : VAL = MakeFst (Pair)

module Snd : VAL = MakeSnd (Pair)


let () = 
	Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x



(*
	$> ocamlopt ex02.ml && ./a.out
	Fst.x = 21, Snd.x = 42
*)


(*
	val fst :'a * 'b -> 'a
	val snd :'a * 'b -> 'b
*)