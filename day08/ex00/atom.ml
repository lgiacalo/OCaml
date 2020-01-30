

class virtual atom (name, symbol, atomic_number) = 
object

	method name 		 : string = name
	method symbol 		 : string = symbol
	method atomic_number : int 	  = atomic_number

	method to_string 		= ("atom (" ^ name ^ ", " ^ symbol ^ ", " ^ (string_of_int atomic_number) ^ ") ")
	method equals (a:atom)	= (name = a#name) && (symbol = a#symbol) && (atomic_number = a#atomic_number)
end

class hydrogen = 
object
	inherit atom ("Hydrogen", "H", 1)
end

class helium = 
object
	inherit atom ("Helium", "He", 2)
end

class lithium = 
object
	inherit atom ("Lithium", "Li", 3)
end

class carbon =
object
	inherit atom ("Carbon", "C", 6)
end

class azote =
object
	inherit atom ("Azote", "N", 7)
end

class oxygen =
object
	inherit atom ("Oxygen", "O", 8)
end

class fluor =
object
	inherit atom ("Fluor", "F", 9)
end

class sodium =
object
	inherit atom ("Sodium", "Na", 11)
end

class fer =
object
	inherit atom ("Fer", "Fe", 26)
end

class cobalt =
object
	inherit atom ("Cobalt", "Co", 27)
end

class nickel =
object
	inherit atom ("Nickel", "Ni", 28)
end

class cuivre =
object
	inherit atom ("Cuivre", "Cu", 29)
end

class zinc =
object
	inherit atom ("Zinc", "Zn", 30)
end



