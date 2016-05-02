open Segment
open Point

type t = E | N of Segment.t * t * t 



let rec parse f bsp p = 
	match bsp with
	| E -> ()
	| N (r, g, d) -> if get_position p r = L then (parse f g p; f r; parse f d p)
					else (parse f d p; f r; parse f g p)

let rec rev_parse f bsp p = match bsp with
	| E -> ()
	| N (r, g, d) -> if not (get_position p r = L) then (rev_parse f g p; f r; rev_parse f d p)
					else (rev_parse f d p; f r; rev_parse f g p)


let rec iter f =  function 
	| E -> ()
	| N (r, g, d) -> iter f g ; f r; iter f d
					


let build_bsp sl = 
	let rec b_bsp = function
		| []-> E
		| x::s -> let (ll, lr) = split x s in N (x, b_bsp ll, b_bsp lr)
	in
	b_bsp sl

