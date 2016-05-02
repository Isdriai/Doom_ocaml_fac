open Options
open Physic
open Point

type t = {
mutable pos : Point.t;
mutable pa : int;
}

let new_player pos pa = let t = {pos = pos ; pa = pa} in t

type dir = Left | Right

let rotate d p = 
	match d with
	| Left -> p.pa <- (p.pa + 10) mod 360
	| Right -> p.pa <- (p.pa - 10) mod 360
	

type mv = MFwd | MBwd | MLeft | MRight

(*on fera attention, l'origine demarre en haut à gauche, il faut donc mettre -10 à y pour "avancer" et 10 pour "reculer"*)
let move d p bsp = 
	if Physic.detect_collision p.pos bsp then
	match d with
	| MFwd -> p.pos <- Point.new_point p.pos.x (p.pos.y-10)
	| MBwd -> p.pos <- Point.new_point p.pos.x (p.pos.y+10)
	| MLeft -> p.pos <- Point.new_point (p.pos.x-10) p.pos.y
	| MRight -> p.pos <- Point.new_point (p.pos.x+10) p.pos.y