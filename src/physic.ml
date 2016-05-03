open Bsp
open Options
open Point
open Segment

let angle (xa, ya) (xb, yb) (xc, yc) =
	(xb -. xa) *. (xc -. xa) +. (yb -. ya) *. (yc -. ya)


let procheSegment p =
	fun s -> let (xc, yc) = float_of_int p.x, float_of_int p.y in
			let (xa, ya) = float_of_int s.porig.x, float_of_int s.porig.y in
			let (xb, yb) = float_of_int s.pdest.x, float_of_int s.pdest.y in
			if sin (acos (angle (xa, ya) (xb, yb) (xc, yc))) < Options.step_dist then raise Exit


let rec secteurProche f bsp p = 
	match bsp with
	| E -> ()
	| N (r, g, d) -> if get_position p r = L then (parse f g p; f r)
					else (parse f d p; f r)

let detect_collision p bsp = 
	try
		secteurProche (procheSegment p) bsp p; false
	with
	| _ -> true
