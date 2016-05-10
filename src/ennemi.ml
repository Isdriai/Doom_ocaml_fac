open Point
open Bsp
open Physic

type ennemi = {id : int ; mutable pos : Point.t; }

let compteur =
	let etat = ref 0 in
	fun () -> etat := !etat + 1; !etat 

let new_ennemi pos = 
	let t = {id = compteur (); pos = pos ;}
	in t

let move_ennemi e pos bsp = 
	if Physic.detect_collision e.pos bsp then bsp
	else(
		let bsp = Bsp.remove_ennemi e.id e.pos bsp in
		e.pos <- pos;
		Bsp.add_ennemi e.id pos bsp)