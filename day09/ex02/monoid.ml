module type MONOID =
sig
	type element

	val zero1 : element
	val zero2 : element
	val mul : element -> element -> element
	val add : element -> element -> element
	val div : element -> element -> element
	val sub : element -> element -> element
end

module INT = 
struct
	type element = int

	let zero1 = 0
	let zero2 = 1

	let add = ( + )
	let sub = ( - )
	let mul = ( * )
	let div = ( / )
end

module FLOAT = 
struct
	type element = int

	let zero1 = 0.
	let zero2 = 1.

	let add = ( +. )
	let sub = ( -. )
	let mul = ( *. )
	let div = ( /. )
end


module type Calc =
	functor (M : MONOID) ->
	sig
		val add : M.element -> M.element -> M.element
		val sub : M.element -> M.element -> M.element
		val mul : M.element -> M.element -> M.element
		val div : M.element -> M.element -> M.element
		val power : M.element -> int -> M.element
		val fact : M.element -> M.element
	end

module Calc =
	functor (M : MONOID) ->
	struct

		let add x y = M.add x y
		let sub x y = M.sub x y
		let mul x y = M.mul x y
		let div x y = M.div x y

		let power x y = let rec loop n acc = match n with
						| nb when (nb = 0) -> acc
						| nb -> loop (n - 1) (M.mul acc x)
					in loop y M.zero2

		let fact x = if (x = M.zero1) then M.zero2 else
					let rec loop n acc = match n with
						| nb when (nb = M.zero1) -> acc
						| nb -> loop (M.sub nb M.zero2) (M.mul acc M.zero2)
					in loop x M.zero2
	end



module Calc_int = Calc(INT)

(* module Calc_float = Calc(FLOAT) *)


let () =
	print_endline (string_of_int (Calc_int.power 3 3));
	print_endline (string_of_int (Calc_int.fact 3 ));
	print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));




(* 		let power x y = let rec loop n acc = match n with
						| nb when (nb = 0) -> (acc)
						| nb -> loop (n - 1) (acc * x)
					in loop y 1 *)

(* 	print_endline ("3^0 = " ^ (string_of_int (power 3 0)));
	print_endline ("3^1 = " ^ (string_of_int (power 3 1)));

	print_endline ("2^3 = " ^ (string_of_int (power 2 3)));
	print_endline ("2^4 = " ^ (string_of_int (power 2 4)));
	print_endline ("2^5 = " ^ (string_of_int (power 2 5)));
	print_endline ("2^6 = " ^ (string_of_int (power 2 6))); *)


	()


