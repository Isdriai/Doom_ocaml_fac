open Options
open Physic
open Point
open Trigo

type t = {
mutable pos : Point.t;
mutable pa : int;
mutable accroupi : bool;
}

let pas = 10.
let d_angle = 3 

let new_player pos pa = let t = {pos = pos ; pa = pa ; accroupi = false} in t

type dir = Left | Right

let rotate d p = 
	match d with
	| Left -> p.pa <- (p.pa + d_angle) mod 360
	| Right -> p.pa <- (p.pa - d_angle) mod 360
	

type mv = MFwd | MBwd | MLeft | MRight

(*
	l'angle 0 corespond à l'état ou le personnage est tourné vers la droite
*)
let move d p bsp = 

	let point_tmp = ref (Point.new_point 0 0) in

	(match d with
		| MFwd -> point_tmp := Point.new_point (p.pos.x+int_of_float(pas*.dcos (p.pa))) 
		(p.pos.y+int_of_float(pas*.dsin (p.pa)))

		| MBwd -> point_tmp := Point.new_point (p.pos.x-int_of_float(pas*.dcos (p.pa))) 
		(p.pos.y-int_of_float(pas*.dsin (p.pa)))

		| MLeft -> point_tmp := Point.new_point (p.pos.x-int_of_float(pas*.dsin (p.pa))) 
		(p.pos.y-int_of_float(pas*.dsin (p.pa-90)))

		| MRight -> point_tmp := Point.new_point (p.pos.x+int_of_float(pas*.dsin (p.pa))) 
		(p.pos.y+int_of_float(pas*.dsin (p.pa-90)))
	);

	if not (Physic.detect_collision !point_tmp bsp) 
	then  p.pos <- !point_tmp
	else () 

let accroupir p = 
	p.accroupi <- not p.accroupi