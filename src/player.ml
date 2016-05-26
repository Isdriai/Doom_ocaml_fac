open Options
open Physic
open Point
open Trigo
open Segment
open Ennemi

type t = {
	mutable pos : Point.t;
	mutable pa : int;
	mutable courir : bool;
	pos_i : Point.t;
	pa_i : int;
    mutable color : Graphics.color;
}

let pas = 10.
let d_angle = 3 

let new_player ?(g = Graphics.rgb 0 50 0) pos pa = 
	let t = {pos = pos ; 
			pa = pa ; 
			courir = false;
			pos_i = pos;
			pa_i = pa;
			color = g;
			} 
	in t

type dir = Left | Right

let rotate ?(angle = d_angle) d p = 
	match d with
	| Left -> p.pa <- (p.pa + angle) mod 360
	| Right -> p.pa <- (p.pa - angle) mod 360
	

type mv = MFwd | MBwd | MLeft | MRight

(*
	l'angle 0 corespond à l'état ou le personnage est tourné vers la droite
*)
let move d p bsp = 

	let point_tmp = ref (Point.new_point 0 0) in

	let float_of_bool b =
		if b then 1. else 0.
	in

	(*si le joueur est en train de courir, il ira plus vite*)

	let nw_pas = pas+.(pas*.float_of_bool p.courir) in

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
	  

let courir p = 
	p.courir <- not p.courir

let reset p = 
	p.pos <- p.pos_i;
	p.pa <- p.pa_i


(* une balle est representer par un segment et un sens
	on la fait avancer et on regarde les collisions
*)
type balle = {mutable emplacement : Segment.t; mutable pa : int; }



let new_balle pos pa = 
	let dir = Point.new_point (pos.x+int_of_float((pas *.dcos pa))) 
		(pos.y+int_of_float(pas *.dsin pa)) in
	let t = {emplacement = Segment.new_segment pos.x pos.y dir.x dir.y; pa = pa;} in
	t

let rec touche balle e = function (*renvoi collison si elle touche un mur*)
	| [] -> ()
	| x::s -> e := x ;
			Segment.dansLaBoite x.position balle.emplacement; touche balle e s 

let tire p ennemi bsp =
	if !ennemi != [] then
		let balle = new_balle p.pos p.pa in
		let e = ref (List.hd !ennemi) in

		let rec trajectoir acc = 
		let (bo, _) = Physic.detect_collision balle.emplacement.pdest bsp in
			if bo then () (*si la balle prend un mur*)
			else if acc = 0 then () (*si la balle part trop loin*)

			else (touche balle e !ennemi; (*on fait bouger la balle avec la meme equation que le joueur*)
				let dest = Point.new_point (balle.emplacement.pdest.x+int_of_float((pas *.dcos (balle.pa)))) 
							(balle.emplacement.pdest.y+int_of_float(pas *.dsin (balle.pa))) in
							
				balle.emplacement <- new_segment balle.emplacement.porig.x balle.emplacement.porig.y
													dest.x dest.y;
			trajectoir (acc - 1))

		in
		try trajectoir 500;bsp
		with Collision -> ennemi := List.filter (fun x -> x != !e) !ennemi; Bsp.remove_ennemi !e.ide !e.position bsp (*on supprime l'ennemi de la list et on renvoi un autre bsp*)
	else bsp