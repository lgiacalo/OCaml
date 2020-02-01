

module type Watchtower =
sig
	type hour = int

	val zero : hour
	val add : hour -> hour -> hour
	val sub : hour -> hour -> hour
end


module Watchtower =
struct
	type hour = int
	
	let zero = 12
	let add x y = (((x + y) mod 12) + 12) mod 12
	let sub x y = (((x - y) mod 12) + 12) mod 12
end

let () = 

  print_int (Watchtower.add 2 (-12));
  print_char '\n';
  print_int (Watchtower.add 12 12);
  print_char '\n';
  print_int (Watchtower.add 0 12);
  print_char '\n';
  print_int (Watchtower.add 0 11);
  print_char '\n';
  print_int (Watchtower.add 99 12);
  print_char '\n';
  print_int (Watchtower.sub 3 12);
  print_char '\n';
  print_int (Watchtower.sub 12 12);
  print_char '\n';
  print_int (Watchtower.sub 12 77);
  print_char '\n';
  print_int (Watchtower.zero);
  print_char '\n'
