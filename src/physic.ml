open Bsp
open Segment

let detect_collision p bsp =
	let rec dc f bsp = 
		match bsp with
		| E -> ()
		| N (r, g, d) -> if get_position p r = L then (dc f g; f r)
						else (dc f d; f r)
	in
	try 
		dc (Segment.dansLaBoite p) bsp; false
	with Exit -> true