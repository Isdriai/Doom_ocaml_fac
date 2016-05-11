open Point
open Bsp
open Physic

type t = {ide : int ; mutable position : Point.t; }

let compteur =
	let etat = ref 0 in
	fun () -> etat := !etat + 1; !etat 

let new_ennemi pos = 
	let t = {ide = compteur (); position = pos ;}
	in t

let move_ennemi e pos bsp = 
	let (detect, _) = Physic.detect_collision e.position bsp in
	if detect then bsp
	else( 
		let bsp = Bsp.remove_ennemi e.ide e.position bsp in
		e.position <- pos;
		Bsp.add_ennemi e.ide pos bsp)