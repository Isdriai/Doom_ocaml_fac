open Segment
open Point
open Trigo
open Player
open Graphics

let calcul_vecteur p s =
	Segment.new_segment (s.porig.x-p.pos.x) 
						(s.porig.y-p.pos.y)
						(s.pdest.x-p.pos.x) 
						(s.pdest.y-p.pos.y)

(*
	x' = x cos a + y sin a
	y' = -x sin a + y cos a
*)

(*à tester demain, voir si les y sont bons,
 et ecris tous les exemple comme il faut sur ta putain de feuille !*)

let calcul_angle p s =
	Segment.new_segment 
		(-truncate (float_of_int (s.porig.x) *. Trigo.dcos p.pa +. float_of_int (-s.porig.y) *. Trigo.dsin p.pa))
		(truncate (float_of_int (-s.porig.x) *. Trigo.dsin p.pa +. float_of_int (s.porig.y) *. Trigo.dcos p.pa))
		(-truncate (float_of_int (s.pdest.x) *. Trigo.dcos p.pa +. float_of_int (-s.pdest.y) *. Trigo.dsin p.pa))
		(truncate (float_of_int (-s.pdest.x) *. Trigo.dsin p.pa +. float_of_int (s.pdest.y) *. Trigo.dcos p.pa))

let ata xo yo xd yd = 
	float_of_int(yd - yo) /. float_of_int(xd - xo)

let clipping s = 

	let xo = s.porig.x in
	let yo = s.porig.y in
	let xd = s.pdest.x in
	let yd = s.pdest.y in

	if xo < 1 && xd < 1 then None
	else if xo < 1 then Some(Segment.new_segment 1 (yo+truncate(float_of_int(1-xo)*. ata xo yo xd yd)) xd yd ) 
	else if xd < 1 then Some(Segment.new_segment xo yo 1 (yd + truncate(float_of_int(1-xd)*. ata xo yo xd yd)))
	else Some(s)

let draw_line xo yo xd yd =
	let pixel = 20 in
	let decal = 50 in
	Graphics.draw_segments [|xo*pixel+decal,yo*pixel+decal,xd*pixel+decal,yd*pixel+decal|]



let affiche p = fun s -> 

	let nw_seg = calcul_angle p (calcul_vecteur p s) in 
	let clip = clipping nw_seg in

	match clip with
	| None -> ()
	| Some(seg) -> 
	(Printf.printf "xa: %d, ya: %d\n xb: %d, yb: %d\nid :%s \n\n" seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y seg.id;
	Graphics.set_color (Graphics.rgb 255 0 0);
	draw_line seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y)

(*faire fenetre graphique et afficher les segments dedans*)

let display bsp p = 

	Bsp.parse (affiche p) bsp p.pos
	
