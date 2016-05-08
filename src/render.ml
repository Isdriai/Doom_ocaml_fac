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

let affiche_segment s = 
	Printf.printf "xa: %d, ya: %d    xb: %d, yb: %d     id: %s\n" s.porig.x s.porig.y s.pdest.x s.pdest.y s.id



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

let draw_line xo yo xd yd =
	let pixel = 20 in
	let decal = 0 in
	Graphics.draw_segments [|xo*pixel+decal,yo*pixel+decal,xd*pixel+decal,yd*pixel+decal|]


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

	(* let ph = 140. in 
	let fh = 0. in 
	let ch = 200. in
	let co = float_of_int co_i in
	let cd = float_of_int cd_i in

	let calcul_max x = 
		let zc =(float_of_int (taille/2)) +. (( ch -. ph )*. float_of_int(d_focale) /. float_of_int(x)) in
		let zf =(float_of_int(taille/2)) +. ( (fh -. ph )*. float_of_int(d_focale) /.float_of_int(x) )in
		(zc, zf)
	in

	let (zuo, zlo) = calcul_max xo in
	let (zud, zld) = calcul_max xd in

	let delta_u = (zud -. zuo ) /. (cd -. co) in
	let delta_l = (zld -. zlo) /. ( cd -. co ) in 

	let algo c zu zl =
		let t = float_of_int taille in 
		if c < 0.-.(t/.2.) then 
		(0., zu -. ( c *. delta_u) , zl -. (c*.delta_l))
		else if c > t-.(t/.2.) then
		(t, zu -. (( c -. t )*.delta_u), zl -. ((c -. t)*.delta_l))
		else 
		(c , zu , zl)
	in

	let (x_ori, y_haut_ori, y_bas_ori) = algo cd zuo zlo in
	let (x_dest, y_haut_dest, y_bas_dest) = algo co zud zld in

	Graphics.set_color (Graphics.rgb 0 100 0);
	Graphics.fill_poly [|
		int_of_float x_ori, int_of_float y_haut_ori;
		int_of_float x_ori, int_of_float y_bas_ori;
		int_of_float x_dest, int_of_float y_bas_dest;
		int_of_float x_dest, int_of_float y_haut_dest;
	|];
	Printf.printf "
	co = %d  cd = %d
	x ori = %f y haut ori = %f \n
	x ori = %f y bas ori = %f \n
	x dest = %f y bas dest = %f \n 
	x dest = %f y haut dest = %f \n\n
	"
		 co_i cd_i
		 x_ori  y_haut_ori
		 x_ori  y_bas_ori
		 x_dest  y_bas_dest
		 x_dest  y_haut_dest;;
 *)







	Printf.printf"co == %d, cd == %d\n"  ;
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
	Printf.printf"
		p_gauche.x == %d  ,p_gauche.y == %d \n;
		p_gauche.x == %d  ,(-p_gauche.y == %d \n);
		p_droite.x == %d  ,p_droite.y == %d \n;
		p_droite.x == %d  ,(-p_droite.y == %d \n)"

		p_gauche.x (hauteur_yeux-(p_droite.y-hauteur_yeux))
		p_gauche.x p_droite.y
		p_droite.x p_gauche.y
		p_droite.x (hauteur_yeux-(p_gauche.y-hauteur_yeux));
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

let projection seg  =

	(*y' = ( ls / 2 ) - (( y * d ) / x *)
	let project p =
		int_of_float(float_of_int (d_focale * p.y) /. float_of_int( p.x )) in

	let cmax = int_of_float (dtan (angle_vision/2) *. float_of_int d_focale) in
	let cmin = (-cmax) in 
	Printf.printf"d == %d \n cmax == %d, cmin == %d\n" d_focale cmax cmin;
	let c_p_orig = project seg.porig in
	let c_p_dest = project seg.pdest in

(*correction devra renvoyer deux points ainsi que deux nouvelles colonnes par rapport à un segment 
et non plus une simple colonne par rapport a une autre colonne*)
	let correction_c cmax cmin ctest = 
		if ctest < cmin then cmin
		else if ctest > cmax then cmax
		else ctest
	in

	match c_p_orig, c_p_dest with
	| a,b when a > cmax && b > cmax -> ()
	| a,b when a < cmin && b < cmin -> ()
	| a,b -> let cor = (correction_c cmax cmin) in 
			passage_3d cmax seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y (cor c_p_dest) (cor c_p_orig)
			(* Printf.printf "d :%d, c1: %d, d : %d, c2 : %d \n\n" d_focale (cor c_p_dest) d_focale (cor c_p_orig); *)
(*
devra renvoyer un quator de points qui representeront les 4 coins du mur à afficher
projection seg -> Point.t * Point.t * Point.t * Point.t
*)

let debug_bsp_2D s p =

    Graphics.draw_segments [|(s.porig.x/4, s.porig.y/4,
    s.pdest.x/4, s.pdest.y/4)
    |]; 
    Printf.printf " origine : %d %d , arrivee : %d %d \n\n"  (s.porig.x/4) (s.porig.y/4) (s.pdest.x/4) (s.pdest.y/4)


let affiche p = fun s -> 
	Printf.printf "commencement ==\n ";
	affiche_segment s;
	let nw_seg = calcul_angle p (calcul_vecteur p s) in 
	Printf.printf "avant clipp ==\n ";
	affiche_segment nw_seg;
	let clip = clipping nw_seg in
	debug_bsp_2D s p;

	match clip with
	| None -> ()
	| Some(seg) -> (projection seg ; 
	(* Printf.printf "xa: %d, ya: %d\n xb: %d, yb: %d\nid :%s \n\n" seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y seg.id;
	Graphics.set_color (Graphics.rgb 255 0 0);
	draw_line seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y *))

(*faire fenetre graphique et afficher les segments dedans*)

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