open Options
open Point
open Segment
open Player
open Bsp
open Render
open Graphics

let affiche_point p = 
	Printf.printf "x: %d, y: %d\n" p.x p.y

let affiche_segment s = 
	Printf.printf "xa: %d, ya: %d    xb: %d, yb: %d     id: %s\n" s.porig.x s.porig.y s.pdest.x s.pdest.y s.id

let affiche_boite_segment s = 
	Printf.printf "xa: %d, ya: %d    xb: %d, yb: %d     id: %s\n" s.boite_gauche_orig.x s.boite_gauche_orig.y s.boite_gauche_dest.x s.boite_gauche_dest.y s.id


let affiche_bsp bsp =
	let rec aff s = function
		| E -> Printf.printf "%s" s; Printf.printf "E\n"
		| N (r, g, d) -> let s2 = s^"|            " in
		 aff s2 g; Printf.printf "%s" s; affiche_segment r; aff s2 d 
	in
	aff "" bsp

let rec iter_cps f bsp=  
	let rec iter cont = function 
	| E -> cont ()
	| N (r, g, d) -> let k  = (fun () -> f r; iter cont d)
					in iter k g
	in	
	iter (fun _ ->()) bsp



let affiche_bsp_bis bsp = 
	iter_cps affiche_segment bsp	

let rec affiche_split (l,r) =
	match l,r with
	| [],[] -> Printf.printf "\n\n"
	| sl::g, sr::d -> affiche_segment sl; 
					  affiche_segment sr;
					  affiche_split (g,d)

	| [], sr::d -> affiche_segment sr; affiche_split ([], d)
	| sl::g , [] ->  affiche_segment sl; affiche_split (g, [])


let construire_liste (_,lab) =
	let rec cl l acc =
		match l with
		 | [] -> acc
		 | (x1, y1, x2, y2)::b -> cl b ((Segment.new_segment x1 y1 x2 y2)::acc)
		in
	cl lab []

let construire_player ((x,y,pa),_) =
	Player.new_player (Point.new_point x y) pa

let affichage2dBoiteCollision bsp =  
	let draw s = Graphics.draw_segments [|s.boite_gauche_orig.x, s.boite_gauche_orig.y, s.boite_gauche_dest.x, s.boite_gauche_dest.y|];
	Graphics.draw_segments [|s.boite_droite_orig.x, s.boite_droite_orig.y, s.boite_droite_dest.x, s.boite_droite_dest.y|]
	 in
	Bsp.iter draw bsp

let affichage2d p bsp =
	let r, g, b = ref 255, ref 0, ref 255 in
	Graphics.set_color (Graphics.rgb !r !g !b);
	Graphics.moveto p.pos.x p.pos.y;
	Graphics.plot p.pos.x p.pos.y; Graphics.draw_string "p";
	let draw s = Graphics.set_color (Graphics.rgb !r !g !b); r := !r - 5;
		Graphics.moveto s.porig.x s.porig.y; Graphics.draw_string s.id; Printf.printf "%s -> " s.id;
		Graphics.draw_segments [|s.porig.x, s.porig.y, s.pdest.x, s.pdest.y|] in
	Bsp.rev_parse draw bsp p.pos


let test lab = 
	let s1 = Segment.new_segment 0 0 1 1 in 
	let s2 = Segment.new_segment 1 0 3 0 in 
	let s3 = Segment.new_segment 2 1 2 3 in 
	let s4 = Segment.new_segment 3 1 4 0 in 
	let s5 = Segment.new_segment 1 1 8 3 in 
	let s6 = Segment.new_segment 8 2 3 3 in 
	let s7 = Segment.new_segment 5 6 5 7 in 
	let s8 = Segment.new_segment 0 0 10 0 in
 	let l_lab = construire_liste lab in 
	let l_seg = s1::s2::s3::s4::s5::s6::s7::s8::[] in
	let test_split = Segment.split s1 l_seg in 
(* 	Printf.printf "bsp :\n\n";
	affiche_bsp b; OK*)
	
	Printf.printf "boite collision :\n\n";
	affiche_boite_segment s8;
	(* Printf.printf "bsp avec iter_cps :\n\n";
	affiche_bsp_bis b; *)

	Printf.printf "affichage split :\n\n";
	affiche_split test_split;



	let p = Point.new_point 3 3 in 
	let player = Player.new_player p 80 in 

	Printf.printf "affichage player :\n\n";


	(*on met le joueur à 90° pr tester ds un cas facile au début*)

	Player.rotate Player.Left player;
	Player.move Player.MFwd player (Bsp.build_bsp []);
	affiche_point player.pos;


	(*on test avec un léger decalage*)

	Player.rotate Left player;
	Player.move Player.MFwd player (Bsp.build_bsp []);
	affiche_point player.pos;

	(* on voit bien que ca marche, voir la sortie, 
	on remet le joueur comme avant *)

	Player.move Player.MBwd player (Bsp.build_bsp []);
	affiche_point player.pos;

	Player.rotate Right player;
	Player.move Player.MBwd player (Bsp.build_bsp []);
	affiche_point player.pos;

	(* on test les pas latéraux, au debut avec un angle de 90° *)

	Player.move MLeft player (Bsp.build_bsp []);
	affiche_point player.pos;

	Player.move MRight player (Bsp.build_bsp []);
	affiche_point player.pos;

	(*Puis avec un décalage*)

	Player.rotate Left player;
	Player.move MLeft player (Bsp.build_bsp []);
	affiche_point player.pos;

	Player.move MRight player (Bsp.build_bsp []);
	affiche_point player.pos;

	Player.move MRight player (Bsp.build_bsp []);
	affiche_point player.pos;

	(*on voit que tout marche 
		il faut avoir ca comme resultats :

		x: 3, y: -7
		x: 2, y: -16
		x: 3, y: -7
		x: 3, y: 3
		x: -7, y: 3
		x: 3, y: 3
		x: -6, y: 4
		x: 3, y: 3
		x: 12, y: 2

	*)


	Printf.printf "affichage display :\n\n";

	let s = string_of_int (850) in 
	let a =  " " ^ s ^ "x" ^ s in
	Graphics.open_graph a;

	 (* let s_d = Segment.new_segment 2 0 2 3 in
	let bd = Bsp.build_bsp (s_d::[]) in
	let playerd = Player.new_player (Point.new_point 0 0) 90 in

	Render.display bd playerd;

	(*resultat attendu = 
		
		xa: 0, ya: -2
 		xb: 3, yb: -2

	*)

	let s_d2 = Segment.new_segment 2 2 4 2 in
	let b2 = Bsp.build_bsp (s_d2::[]) in
	let player2 = Player.new_player (Point.new_point 1 1) 90 in

	(* Render.display b2 player2; 

	resultat attendu = 
		
		xa: 1, ya: -1
 		xb: 1, yb: -3 *)

	

	let s_d3 = Segment.new_segment (-3) 1 (-1) 3 in
	let b3 = Bsp.build_bsp (s_d3::[]) in
	let player3 = Player.new_player (Point.new_point (-1) 1) 135 in

	Render.display b3 player3; *)

	(*
	resultat attendu =

		xa: 1, ya: 1
		xb: 1, yb, -1
		attention normalement c'est pas ca 
		mais avec les histoires de int/float c'est moins précis

		Tout est ok
	*) 

	
	List.iter affiche_segment l_lab;
	let bsp_test = Bsp.build_bsp l_lab in 
	let player_test = Player.new_player (Point.new_point 350 150) 90 in 
	affiche_bsp bsp_test; 
	affichage2dBoiteCollision bsp_test;
	Printf.printf "\n\n";
	affichage2d player_test bsp_test

	(* Render.display bsp_test p_lab ; *)
	
;;