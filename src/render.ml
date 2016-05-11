open Segment
open Point
open Trigo
open Player
open Graphics
open Ennemi


let taille = 500

let int_of_bool b = 
	if b then 1 else 0

let accroupir = ref false

let angle_vision = Options.fov

let fabs a =
	if a < 0. then -.a else a

let d_focale = int_of_float(float_of_int(taille/2)/. fabs (dtan (angle_vision/2 ))) 

(*Effectue une translation sur un segment par rapport à la position du joueur*)
let calcul_vecteur p s =
	Segment.new_segment ~s:s.id_autre
						(s.porig.x-p.pos.x) 
						(s.porig.y-p.pos.y)
						(s.pdest.x-p.pos.x) 
						(s.pdest.y-p.pos.y)

(*
	on effectue une rotation sur les segments pour les avoir en face du joueur

	les formules =
	xa' = xa * cos (-a) - ya * sin (-a)
	ya' = xa * sin (-a) + ya * cos (-a)
	xb' = xb * cos (-a) - yb * sin (-a)
	yb' = xb * sin (-a) + yb * cos (-a)
*)

let calcul_angle p s =
	Segment.new_segment 
		~s:s.id_autre
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
(*on affiche pas un mur qui serait derriere le joueur, si il y a une partie du mur qui se trouve derriere,
nous créons un nouveau segment*)
	if xo < 1 && xd < 1 then None
	else if xo < 1 then Some(Segment.new_segment 1 (yo+int_of_float(float_of_int(1-xo)*. angle_mur)) xd yd ) 
	else if xd < 1 then Some(Segment.new_segment xo yo 1 (yd + int_of_float(float_of_int(1-xd)*. angle_mur)))
	else Some(s)

let distance xa ya xb yb =
	let xf = float_of_int xa in
	let yf = float_of_int xb in
	let tf = float_of_int xb in
	let zf = float_of_int yb in


	sqrt ((xf**2. -. tf**2. )+.(yf**2. -. zf**2. ))

(*
on calcule la correspondance entre la projection et la coordonnée x de l'affichage, 
ce qui revient à faire une fonction affine
*)
let calcul_p_x cmax c =
	let a = float_of_int (taille/2)/.float_of_int(-cmax) in 
	let b = float_of_int(taille/2) in
	int_of_float ( a *. float_of_int (c) +. b)

(*va afficher en 3D un segment*)
let passage_3d color cmax xo yo xd yd co cd =

	let accroupissement = 
		int_of_bool (!accroupir) * 25 
	in
	let hauteur_yeux = (taille/2) + accroupissement in 

(*on fait la meme chose que pour x, on applique une simple fonction affine *)
	let calcul_p_y x y =
		let echelle = float_of_int(taille/4) in
		let rapport = float_of_int d_focale /. distance x y 0 0 in
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

	Graphics.set_color (color);

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

let projection ?(c = Graphics.rgb 0 50 0) seg p =

	(*y' = ( y * d ) / x
	cette formule permet de connaitre la colonne sur l'ecran d'une extremité d'un segment
	 *)
	let project p =
		int_of_float(float_of_int (d_focale * p.y) /. float_of_int( p.x )) in

	let cmax = int_of_float (dtan (angle_vision/2) *. float_of_int d_focale) in
	let cmin = (-cmax) in 
	let c_p_orig = project seg.porig in
	let c_p_dest = project seg.pdest in

(*Si le segment à une partie en dehors du champ de vision du joueur, on affiche seulement le pan visible,
pour cela, on calcul le point d'intersection entre le champ de vision et le segment, ce que fait correction*)
	let correction point ctest = 

		if ctest < cmin then 
			let (nw_x,nw_y) = Trigo.point_intersection_droites seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y 0 0 d_focale cmin in
			nw_x,nw_y,cmin
		else if ctest > cmax then 
			let (nw_x,nw_y) = Trigo.point_intersection_droites seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y 0 0 d_focale cmax in
			nw_x,nw_y,cmax
		else point.x, point.y, ctest
	in

	match c_p_orig, c_p_dest with
	| a,b when a > cmax && b > cmax -> ()
	| a,b when a < cmin && b < cmin -> ()
	| a,b -> let (nw_x_orig,nw_y_orig,nw_c_p_orig) = correction seg.porig c_p_orig in
			let (nw_x_dest,nw_y_dest,nw_c_p_dest) = correction seg.pdest c_p_dest in 
			let col = ref c in
			(if seg.id_autre = 0 then () else col := Graphics.rgb 0 0 0);
			passage_3d !col cmax nw_x_orig nw_y_orig nw_x_dest nw_y_dest nw_c_p_dest nw_c_p_orig

			
			



let affiche_mp seg t =

	let emplacement = float_of_int (taille/6) in
		let xo = float_of_int seg.porig.x in
		let yo = float_of_int seg.porig.y in
		let xd = float_of_int seg.pdest.x in
		let yd = float_of_int seg.pdest.y in
		Graphics.draw_segments [|
		int_of_float((xo *. emplacement /.t )+. emplacement),
		int_of_float((yo *. emplacement /.t )+. emplacement),
		int_of_float((xd *. emplacement /.t )+. emplacement),
		int_of_float((yd *. emplacement /.t )+. emplacement)|] 

let mini_map perso s =
	Graphics.set_color (Graphics.rgb 0 40 40);
	let nw_perso = Player.new_player perso.pos (perso.pa - 90) in
	let seg = calcul_angle nw_perso (calcul_vecteur perso s) in
	affiche_mp seg (float_of_int taille)

let pre_map () = 
	Graphics.set_color (Graphics.rgb 0 0 40);
	Graphics.fill_poly [|(0,0);
	0,(taille/4);
	(taille/4),(taille/4);
	(taille/4),0|]

let affiche p = fun s -> 

	let nw_seg = calcul_angle p (calcul_vecteur p s) in 
	let clip = clipping nw_seg in

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

let viseur () =
	Graphics.set_color (Graphics.rgb 250 250 250);
	let viseur = 5 in
	let t = taille/2 in
	Graphics.draw_segments [|
		t, t + viseur,
		t, t - viseur;
		t-viseur, t,
		t+viseur, t
	|]

let affiche_ennemi player (id,posi) =
	let gros = sqrt(float_of_int(Options.distance_mur/2)) in
	let alpha = try atan (float_of_int(posi.x/posi.y))
				with Division_by_zero -> 0.
			in

	let s = Segment.new_segment (posi.x- int_of_float(gros *.sin alpha))
								(posi.y- int_of_float(gros *.cos alpha))
								(posi.x+ int_of_float(gros *.sin alpha))
								(posi.y+ int_of_float(gros *.cos alpha))
							in
	let nw_s = clipping(calcul_angle player (calcul_vecteur player s)) in
	match nw_s with
	| None -> ()
	| Some(seg) -> projection ~c:(Graphics.rgb 50 0 0) seg player

let display bsp p = 

	accroupir := p.accroupi;
	clear_graph ();
	Bsp.rev_parse ~h:(affiche_ennemi p) (affiche p)  bsp p.pos;
	Printf.printf "position joueur %d, %d\n" p.pos.x p.pos.y;
	(* pre_map ();
	Bsp.iter  (mini_map p) bsp; *)
	viseur () ;
	synchronize ()


