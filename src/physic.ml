open Bsp
open Segment

let detect_collision p bsp =

	let seg = ref (Segment.new_segment 0 0 0 0) in

	let rec dc f bsp = 
		match bsp with
		| E -> ()
		| Ennemi(_) -> ()
		| N (r, g, d) -> seg := r;
						if get_position p r = L then (dc f g; f r)
						else (dc f d; f r)
	in
	try 
		dc (Segment.dansLaBoite p) bsp; (false, None)
	with Exit -> 
		if (!seg).id_autre = 0 then
			(true, None)
		else
			(true, Some(!seg))