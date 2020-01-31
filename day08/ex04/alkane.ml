
class alkane (n:int) = 
object
	inherit Molecule.molecule ((match n with 
		| 1 -> "Meth" | 2 -> "Eth" | 3 -> "Prop" | 4 -> "But" | 5 -> "Pent" | 6 -> "Hex"
        | 7 -> "Hept" | 8 -> "Oct" | 9 -> "Non" | 10 -> "Dec" | 11 -> "Undec" | 12 -> "Dodec" 
        | _ -> invalid_arg "n doit etre entre [1, 12]") ^ "ane",
        (List.init n (fun x -> (new Atom.carbon))) @ (List.init (n * 2 + 2) (fun x -> (new Atom.hydrogen)))
    )
end

class methane =
object
	inherit alkane 1
end

class ethane =
object
	inherit alkane 2
end

class propane =
object
	inherit alkane 3
end

class butane =
object
	inherit alkane 4
end

class pentane =
object
	inherit alkane 5
end

class hexane =
object
	inherit alkane 6
end

class heptane =
object
	inherit alkane 7
end

class octane =
object
	inherit alkane 8
end
