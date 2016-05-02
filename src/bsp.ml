open Segment
open Point

type t = E | N of Segment.t * t * t 

let gauche segment point=
	let res = (segment.pdest.x - segment.porig.x) * (point.y - segment.porig.y) - (segment.pdest.y - segment.porig.y)* (point.x - segment.porig.x) in
	if res > 0 then true
	else false

let rec parse f bsp p = 
	match bsp with
	| E -> ()
	| N (r, g, d) -> if gauche r p then (parse f g p; f r; parse f d p)
					else (parse f d p; f r; parse f g p)

let rec rev_parse f bsp p = match bsp with
	| E -> ()
	| N (r, g, d) -> if not (gauche r p) then (rev_parse f g p; f r; rev_parse f d p)
					else (rev_parse f d p; f r; rev_parse f g p)


let iter f =  function 
	| E -> ()
	| N (r, g, d) -> parse f g ; f r; parse f d p
					

let build_bsp sl = failwith "TODO"
