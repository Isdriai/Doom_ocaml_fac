open Segment
open Point
open Trigo
open Player
open Graphics

let taille = 500

let angle_vision = 90

let calcul_vecteur p s =
	Segment.new_segment (s.porig.x-p.pos.x) 
						(s.porig.y-p.pos.y)
						(s.pdest.x-p.pos.x) 
						(s.pdest.y-p.pos.y)

(*
	x' = x cos a + y sin a
	y' = -x sin a + y cos a
*)

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
	let decal = 0 in
	Graphics.draw_segments [|yo*pixel+decal,xo*pixel+decal,yd*pixel+decal,xd*pixel+decal|]

let correction_y ymax ymin ytest = 
	if ytest < ymin then ymin
	else if ytest > ymax then ymax
	else ytest


let projection seg  =
	let d_focale = truncate(float_of_int(taille)/.tan ((float_of_int angle_vision)/. 2.)) in 
	let ymax = truncate((dtan (angle_vision/2)) *. float_of_int(d_focale)) in
	let ymin = (-ymax) in 
	let ls = ymax - ymin in 
	let y_p_orig = truncate((float_of_int ls /. 2.) -. (float_of_int(seg.porig.y - d_focale) /. float_of_int(seg.porig.x))) in
	let y_p_dest = truncate((float_of_int ls /. 2.) -. (float_of_int(seg.pdest.y - d_focale) /. float_of_int(seg.pdest.x))) in

	match y_p_orig, y_p_dest with
	| a,b when a > ymax && b > ymax -> Printf.printf "ymax: %d, ymin: %d, a :%d, b: %d\n\n" ymax ymin a b
	| a,b when a < ymin && b < ymin -> Printf.printf "ymax: %d, ymin: %d, a :%d, b: %d\n\n" ymax ymin a b
	| a,b -> let cor = (correction_y ymax ymin) in 
			Printf.printf "x :%d, y1: %d, x : %d, y2 : %d \n\n" d_focale (cor y_p_dest) d_focale (cor y_p_orig)


let affiche p = fun s -> 

	let nw_seg = calcul_angle p (calcul_vecteur p s) in 
	let clip = clipping nw_seg in

	match clip with
	| None -> ()
	| Some(seg) -> 
	(projection seg ;
	Printf.printf "xa: %d, ya: %d\n xb: %d, yb: %d\nid :%s \n\n" seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y seg.id;
	Graphics.set_color (Graphics.rgb 255 0 0);
	draw_line seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y)

(*faire fenetre graphique et afficher les segments dedans*)

let display bsp p = 

	Bsp.parse (affiche p) bsp p.pos
	
