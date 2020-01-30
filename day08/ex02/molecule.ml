
class virtual molecule (name, list_atom) = 
object(self)

	val _list_atom : Atom.atom list = list_atom

	method name 		 : string 	= name
	method formula		 : string	= if _list_atom = [] then "" else self#transform (self#reagence (self#encode _list_atom))

	method to_string			= ("molecule (" ^ name ^ ", " ^ self#formula ^ ") ")
	method equals (m:molecule)	= (m#formula = self#formula)


	method private encode liste = match liste with
		| []	-> 	[]
		| _		->	let rec loop lst lstt acc = match lst with
			| first::second::last when first#symbol = second#symbol -> loop (second::last) lstt (acc + 1)
			| first::last -> loop last (lstt @ [(first#symbol, acc)]) 1
			| [] -> lstt
		in loop liste [] 1

	method private isAtomx x (s, n) = (s = x)
	method private isNotAtomx x (s, n) = (s <> x)
	method private reagence liste  = if ((List.find_opt (self#isAtomx "C") liste) = None)
		then List.sort compare liste
		else let c = (List.find (self#isAtomx "C") liste) in
			let h = match (List.find_opt (self#isAtomx "H") liste) with
				| None -> [c] @ (List.filter (self#isNotAtomx "C") liste)
				| Some (_) -> [c] @ [((List.find (self#isAtomx "H") liste))] @ ((List.filter (self#isNotAtomx "C") (List.filter (self#isNotAtomx "H") liste)))
			in h

	method private transform liste = let rec loop l form = match l with
		| [] -> form
		| (s, n)::t -> if (n = 1) then loop t (form ^ s)
				else loop t (form ^ s ^ (string_of_int n))
		in loop liste ""
end



class water =
object
	inherit molecule ("Water",
	     let h = new Atom.hydrogen in
	     let o = new Atom.oxygen in
	     [o; h; h]
	)
end


class carbon_dioxyde =
object
	inherit molecule ("Carbon dioxyde",
	     let c = new Atom.carbon in
	     let o = new Atom.oxygen in
	     [o; o; c]
	)
end


class trinitrotoluene =
object
	inherit molecule ("Trinitrotoluene",
	     let h = new Atom.hydrogen in
	     let o = new Atom.oxygen in
	     let n = new Atom.nitrogen in
	     let c = new Atom.carbon in
	     [n; n; n; h; h; h; h; h; o; o; o; o; o; o; c; c; c; c; c; c; c]
	)
end


class glucose =
object
	inherit molecule ("Glucose",
	     let h = new Atom.hydrogen in
	     let o = new Atom.oxygen in
	     let c = new Atom.carbon in
	     [o; o; o; o; o; o; c; c; c; c; c; c; h; h; h; h; h; h; h; h; h; h; h; h]
	)
end

class alanine =
object
	inherit molecule ("Alanine",
	     let h = new Atom.hydrogen in
	     let o = new Atom.oxygen in
	     let n = new Atom.nitrogen in
	     let c = new Atom.carbon in
	     [n; h; h; h; h; h; h; h; o; o; c; c; c]
	)
end

class ethanol =
object
	inherit molecule ("Ethanol",
	     let h = new Atom.hydrogen in
	     let o = new Atom.oxygen in
	     let c = new Atom.carbon in
	     [o; c; c; h; h; h; h; h; h]
	)
end

class sucre =
object
	inherit molecule ("Sucre",
	     let h = new Atom.hydrogen in
	     let o = new Atom.oxygen in
	     let c = new Atom.carbon in
	     [o; o; o; o; o; o; o; o; o; o; o; c; c; c; c; c; c; c; c; c; c; c; c; h; h; h; h; h; h; h; h; h; h; h; h; h; h; h; h; h; h; h; h; h; h]
	)
end




