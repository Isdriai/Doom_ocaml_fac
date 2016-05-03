open Options
open Physic
open Point

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

let pi = 3.1415926535897932384626433832795

let radian_of_deg deg = 
	pi *. (float_of_int deg) /. 180.

(*on fera attention, l'origine demarre en haut à gauche, il faut donc mettre -10 à y pour "avancer" et 10 pour "reculer"
	l'angle 0 corespond à l'état ou le personnage est tourné vers la droite
*)
let move d p bsp = 

	let player_tmp = new_player(Point.new_point 0 0) p.pa in

	match d with
	| MFwd -> player_tmp.pos <- Point.new_point (p.pos.x+truncate(pas*.cos (radian_of_deg p.pa))) 
										(p.pos.y-truncate(pas*.sin (radian_of_deg p.pa)))

	| MBwd -> player_tmp.pos <- Point.new_point (p.pos.x-truncate(pas*.cos (radian_of_deg p.pa))) 
										(p.pos.y+truncate(pas*.sin (radian_of_deg p.pa)))

	| MLeft -> player_tmp.pos <- Point.new_point (p.pos.x-truncate(pas*.sin (radian_of_deg p.pa))) 
										(p.pos.y-truncate(pas*.cos (radian_of_deg p.pa)))

	| MRight -> player_tmp.pos <- Point.new_point (p.pos.x+truncate(pas*.sin (radian_of_deg p.pa))) 
										(p.pos.y+truncate(pas*.cos (radian_of_deg p.pa)))
	;

	if not (Physic.detect_collision player_tmp.pos bsp) 
	then p.pos <- player_tmp.pos
	else ()