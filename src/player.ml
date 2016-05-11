open Options
open Physic
open Point
open Trigo
open Segment
open Ennemi

type t = {
mutable pos : Point.t;
mutable pa : int;
mutable accroupi : bool;
pos_i : Point.t;
pa_i : int;
}

let pas = 10.
let d_angle = 3 

let new_player pos pa = 
	let t = {pos = pos ; 
			pa = pa ; 
			accroupi = false;
			pos_i = pos;
			pa_i = pa;
			} 
	in t

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

	let float_of_bool b =
		if b then 1. else 0.
	in

	(*si le joueur est accroupi, il ira moins vite*)

	let nw_pas = pas-.(pas*.float_of_bool p.accroupi)/.2. in

	(match d with
		| MFwd -> point_tmp := Point.new_point (p.pos.x+int_of_float((nw_pas*.dcos (p.pa)))) 
		(p.pos.y+int_of_float(nw_pas*.dsin (p.pa)))

		| MBwd -> point_tmp := Point.new_point (p.pos.x-int_of_float((nw_pas*.dcos (p.pa)))) 
		(p.pos.y-int_of_float(nw_pas*.dsin (p.pa)))

		| MLeft -> point_tmp := Point.new_point (p.pos.x-int_of_float((nw_pas*.dsin (p.pa)))) 
		(p.pos.y-int_of_float(nw_pas*.dsin (p.pa-90)))

		| MRight -> point_tmp := Point.new_point (p.pos.x+int_of_float((nw_pas*.dsin (p.pa)))) 
		(p.pos.y+int_of_float(nw_pas*.dsin (p.pa-90)))
	);

	let coli = (Physic.detect_collision !point_tmp bsp) in 
	match coli with
	| (false, _) -> p.pos <- !point_tmp
	| (true, Some(seg)) -> p.pos <- Point.new_point ((seg.porig.x + seg.pdest.x)/2) ((seg.porig.y + seg.pdest.y)/2)
	| (true, None) -> ()
	  

let accroupir p = 
	p.accroupi <- not p.accroupi

let reset p = 
	p.pos <- p.pos_i;
	p.pa <- p.pa_i


(* let balle_dans_le_mur tire s = (* Si tout est a gauche (ou droite)*)
	let (a,b) = Segment.split_segment tire s in
	if a = None || b = None

let tire p ennemi bsp =
	let ligne_visu = Segment.new_segment p.pos.x p.pos.y ennemi.position.x ennemi.position.y in
	let orientation_joueur = (Point.new_point (p.pos.x-int_of_float((10*.dcos (p.pa)))) 
		(p.pos.y-int_of_float(10*.dsin (p.pa)))) in

	if Segment.get_position orientation_joueur ligne_visu = C then (let (a,b) = Segment.split_segment )
		if  *)


(* une balle est representer par un segment et un sense
	on la fait avancer et on regarde les collisions
*)
type balle = {mutable emplacement : Segment.t; mutable pa : int; }

let new_balle pos pa = 
	let dir = Point.new_point (pos.x+int_of_float((pas *.dcos pa))) 
		(pos.y+int_of_float(pas *.dsin pa)) in
	let t = {emplacement = Segment.new_segment pos.x pos.y dir.x dir.y; pa = pa;} in
	t
let rec touche balle = function
	| [] -> ()
	| x::s -> Segment.dansLaBoite x.position balle.emplacement; touche balle s

let tire p ennemi bsp =
	let balle = new_balle p.pos p.pa in

	let rec trajectoir acc = 
	let (bo, _) = Physic.detect_collision balle.emplacement.pdest bsp in
		if bo then  ()
		else (touche balle ennemi;
			let dest = Point.new_point (balle.emplacement.pdest.x+int_of_float((pas *.dcos (balle.pa)))) 
						(balle.emplacement.pdest.y+int_of_float(pas *.dsin (balle.pa))) in
			balle.emplacement <- new_segment balle.emplacement.porig.x balle.emplacement.porig.y
												dest.x dest.y)

	in
	try trajectoir 500
	with Exit -> ()