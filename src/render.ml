open Segment
open Point
open Trigo
open Player

let calcul_vecteur p s =
	Segment.new_segment (s.porig.x-p.pos.x) 
						(s.porig.y-p.pos.y)
						(s.pdest.x-p.pos.x) 
						(s.pdest.y-p.pos.y)

(*x' = x cos a + y sin a
  y' = -x sin a + y cos a
*)

let calcul_angle p s =
	Segment.new_segment 
		(truncate (float_of_int (s.porig.x) *. Trigo.dcos p.pa +. float_of_int (s.porig.y) *. Trigo.dsin p.pa))
		(truncate (float_of_int (-s.porig.x) *. Trigo.dsin p.pa +. float_of_int (-s.porig.y) *. Trigo.dcos p.pa))
		(truncate (float_of_int (s.pdest.x) *. Trigo.dcos p.pa +. float_of_int (s.pdest.y) *. Trigo.dsin p.pa))
		(truncate (float_of_int (-s.pdest.x) *. Trigo.dsin p.pa +. float_of_int (-s.pdest.y) *. Trigo.dcos p.pa))

let affiche p = fun s -> 
	let nw_seg = calcul_angle p (calcul_vecteur p s) in 
	Printf.printf "xa: %d, ya: %d\n xb: %d, yb: %d\n\n" nw_seg.porig.x nw_seg.porig.y nw_seg.pdest.x nw_seg.pdest.y


let display bsp p = 
	Bsp.parse (affiche p) bsp p.pos
