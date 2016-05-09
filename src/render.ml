open Segment
open Point
open Trigo
open Player
open Graphics


let taille = 500


let angle_vision = 45

let fabs a =
	if a < 0. then -.a else a

let d_focale = int_of_float(float_of_int(taille/2)/. fabs (dtan (angle_vision/2 ))) 

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
		(int_of_float (float_of_int (s.porig.x) *. Trigo.dcos (-p.pa) -. float_of_int (s.porig.y) *. Trigo.dsin (-p.pa)))
		(int_of_float (float_of_int (s.porig.x) *. Trigo.dsin (-p.pa) +. float_of_int (s.porig.y) *. Trigo.dcos (-p.pa)))
		(int_of_float (float_of_int (s.pdest.x) *. Trigo.dcos (-p.pa) -. float_of_int (s.pdest.y) *. Trigo.dsin (-p.pa)))
		(int_of_float (float_of_int (s.pdest.x) *. Trigo.dsin (-p.pa) +. float_of_int (s.pdest.y) *. Trigo.dcos (-p.pa)))
	
let ata xo yo xd yd = 
	float_of_int(yd - yo) /. float_of_int(xd - xo)


let clipping s = 

	let xo = s.porig.x in
	let yo = s.porig.y in
	let xd = s.pdest.x in
	let yd = s.pdest.y in
	let angle_mur = ata xo yo xd yd in

	if xo < 1 && xd < 1 then None
	else if xo < 1 then Some(Segment.new_segment 1 (yo+int_of_float(float_of_int(1-xo)*. angle_mur)) xd yd ) 
	else if xd < 1 then Some(Segment.new_segment xo yo 1 (yd + int_of_float(float_of_int(1-xd)*. angle_mur)))
	else Some(s)

let distance x y =
	let xf = float_of_int x in
	let yf = float_of_int y in

	sqrt ((xf**2.)+.(yf**2.))

(*
on calcule la correspondance entre la projection et la coordonnée x de l'affichage, 
ce qui revient à faire une fonction affine
*)
let calcul_p_x cmax c =
	let a = float_of_int(taille)/. (dtan (angle_vision/2) *. float_of_int(d_focale)) in 
	let b = float_of_int(taille/2) in
	int_of_float ( float_of_int (taille/2)/.float_of_int(-cmax) *. float_of_int (c) +. float_of_int (taille/2))


let passage_3d cmax xo yo xd yd co cd =

	let hauteur_yeux = taille/2 in 

	let calcul_p_y x y =
		let limite = taille in 
		let echelle = float_of_int(taille/4) in
		let rapport = float_of_int d_focale /. distance x y in
		let calcul = int_of_float(echelle *. rapport)+hauteur_yeux in 
		calcul
	in

	let p_gauche = Point.new_point
	(calcul_p_x cmax co)
	(calcul_p_y xo yo)
	in
	let p_droite = Point.new_point
	(calcul_p_x cmax cd)
	(calcul_p_y xd yd) in 
	Graphics.set_color (Graphics.rgb 0 100 0);
	Graphics.fill_poly [|
		p_gauche.x,(hauteur_yeux-(p_droite.y-hauteur_yeux));
		p_gauche.x,p_droite.y;
		p_droite.x,p_gauche.y;
		p_droite.x,(hauteur_yeux-(p_gauche.y-hauteur_yeux))
	|];
	Graphics.set_color (Graphics.rgb 0 0 0);
	Graphics.draw_segments [|
		p_gauche.x,(hauteur_yeux-(p_droite.y-hauteur_yeux)),p_gauche.x,p_droite.y;
		p_droite.x,p_gauche.y,p_droite.x,(hauteur_yeux-(p_gauche.y-hauteur_yeux));
		p_gauche.x,(hauteur_yeux-(p_droite.y-hauteur_yeux)), p_droite.x,(hauteur_yeux-(p_gauche.y-hauteur_yeux));
		p_gauche.x,p_droite.y,p_droite.x,p_gauche.y;
	|]

let projection seg p =

	(*y' = ( ls / 2 ) - (( y * d ) / x *)
	let project p =
		int_of_float(float_of_int (d_focale * p.y) /. float_of_int( p.x )) in

	let cmax = int_of_float (dtan (angle_vision/2) *. float_of_int d_focale) in
	let cmin = (-cmax) in 
	let c_p_orig = project seg.porig in
	let c_p_dest = project seg.pdest in

(*correction devra renvoyer deux points ainsi que deux nouvelles colonnes par rapport à un segment 
et non plus une simple colonne par rapport a une autre colonne*)
	let correction point ctest = 

		let point_intersection_droites s d =
			let xa = float_of_int s.porig.x in 
			let xb = float_of_int s.pdest.x in 
			let ya = float_of_int s.porig.y in 
			let yb = float_of_int s.pdest.y in 
			let xc = float_of_int d.porig.x in 
			let xd = float_of_int d.pdest.x in 
			let yc = float_of_int d.porig.y in 
			let yd = float_of_int d.pdest.y in 

			let dd = ((xb -. xa) *. (yd -. yc) -. (yb -. ya) *. (xd -. xc)) in

			let r = ((ya -. yc) *. (xd -. xc) -. (xa -. xc) *. (yd -. yc)) /. dd in 
			let xi = truncate (xa +. r *. (xb -. xa))  in 
		    let yi = truncate (ya +. r *. (yb -. ya))  in 
		    (xi,yi)

		in

		if ctest < cmin then 
		let (nw_x,nw_y) = point_intersection_droites seg (Segment.new_segment 0 0 (d_focale) cmin) in
		nw_x,nw_y,cmin
		else if ctest > cmax then 
		let (nw_x,nw_y) = point_intersection_droites seg (Segment.new_segment 0 0 (d_focale) cmax) in
		nw_x,nw_y,cmax
		else point.x, point.y, ctest
	in

	match c_p_orig, c_p_dest with
	| a,b when a > cmax && b > cmax -> ()
	| a,b when a < cmin && b < cmin -> ()
	| a,b -> let (nw_x_orig,nw_y_orig,nw_c_p_orig) = correction seg.porig c_p_orig in
			let (nw_x_dest,nw_y_dest,nw_c_p_dest) = correction seg.pdest c_p_dest in 
			passage_3d cmax nw_x_orig nw_y_orig nw_x_dest nw_y_dest nw_c_p_dest nw_c_p_orig

(*
devra renvoyer un quator de points qui representeront les 4 coins du mur à afficher
projection seg -> Point.t * Point.t * Point.t * Point.t
*)
let debug_bsp_2D s p =

    Graphics.draw_segments [|(s.porig.x/4, s.porig.y/4,
    s.pdest.x/4, s.pdest.y/4)
    |]


let affiche p = fun s -> 

	let nw_seg = calcul_angle p (calcul_vecteur p s) in 
	let clip = clipping nw_seg in
	debug_bsp_2D s p;

	match clip with
	| None -> ()
	| Some(seg) -> projection seg p

let clear_graph () = 
	Graphics.set_color (Graphics.rgb 40 40 40);
	Graphics.fill_poly[|
	0,0;
	0,taille/2;
	taille,taille/2;
	taille,0;
	|];
	Graphics.set_color (Graphics.rgb 75 0 0);
	Graphics.fill_poly[|
	0,taille;
	0,taille/2;
	taille,taille/2;
	taille,taille;
	|]

let display bsp p = 

	clear_graph ();
	Bsp.rev_parse (affiche p) bsp p.pos;
	synchronize ()