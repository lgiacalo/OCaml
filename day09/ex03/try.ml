module type Try = 
sig
	type 'a t

	val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val recover : 'a t -> (exn -> 'a t) -> 'a t
    val filter : 'a t -> ('a -> bool) -> 'a t
    val flatten : 'a t t -> 'a t
end

module Try = 
struct
	type 'a t = Success of 'a | Failure of exn 

	let return t = Success t
	let bind t f = match t with
				| Failure (e) -> Failure e
				| Success (x) -> f x

    let recover t f = match t with
				| Failure (e) -> f e
				| Success (x) -> Success x

    let filter t f = match t with
    			| Failure (e) -> Failure e
    			| Success (x) -> 
    				if ((f x) = false) then Failure (Failure "Fail")
    				else Success x

    let flatten tt = match tt with
    			| Failure (e) -> Failure e
    			| Success (x) -> match x with 
    						| Failure (e) -> Failure e
    						| Success (y) -> Success y

end




