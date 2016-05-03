open Bsp

let surSegment p s =
	get_position p s && new_segment

let detect_collision p bsp = 

	let d_t pre = function
	 match bsp with
	 | E -> SurSegment p pre
	 | N (r, g, d) -> if get_position p r = L then detect_collision p pre g
					else detect_collision p pre d
	in

	d_t (new_segment 0 0 0 0) bsp