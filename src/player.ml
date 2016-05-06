open Options
open Physic
open Point
open Trigo

type t = {
mutable pos : Point.t;
mutable pa : int;
}

let pas = 10.

let new_player pos pa = let t = {pos = pos ; pa = pa} in t

type dir = Left | Right

let rotate d p = 
	match d with
	| Left -> p.pa <- (p.pa + 10) mod 360
	| Right -> p.pa <- (p.pa - 10) mod 360
	

type mv = MFwd | MBwd | MLeft | MRight

(*on fera attention, l'origine demarre en haut à gauche, il faut donc mettre -10 à y pour "avancer" et 10 pour "reculer"
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
		(p.pos.y-int_of_float(pas*.dcos (p.pa)))

		| MRight -> point_tmp := Point.new_point (p.pos.x+int_of_float(pas*.dsin (p.pa))) 
		(p.pos.y+int_of_float(pas*.dcos (p.pa)))
	);

	if not (Physic.detect_collision !point_tmp bsp) 
	then  p.pos <- !point_tmp
	else () 