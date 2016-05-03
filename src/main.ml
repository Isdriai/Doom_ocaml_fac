open Options
open Point
open Segment
open Player
open Bsp
open Render

let affiche_point p = 
	Printf.printf "x: %d, y: %d\n" p.x p.y

let affiche_segment s = 
	Printf.printf "xa: %d, ya: %d    xb: %d, yb: %d\n" s.porig.x s.porig.y s.pdest.x s.pdest.y

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

let () = 
	let s1 = Segment.new_segment 1 1 0 0 in 
	let s2 = Segment.new_segment 1 0 3 0 in 
	let s3 = Segment.new_segment 2 1 2 3 in 
	let s4 = Segment.new_segment 3 1 4 0 in 
	let s5 = Segment.new_segment 1 1 8 3 in 
	let s6 = Segment.new_segment 8 2 3 3 in 
	let s7 = Segment.new_segment 5 6 5 7 in 
	let s8 = Segment.new_segment 0 10 0 0 in
 	let test_split_segment = Segment.split_segment s1 s3 in 
 
	let l_seg = s1::s2::s3::s4::s5::s6::s7::s8::[] in
	let test_split = Segment.split s1 l_seg in 
	let b = Bsp.build_bsp l_seg in 

	Printf.printf "bsp :\n\n";
	affiche_bsp b;

	Printf.printf "bsp avec iter_cps :\n\n";
	affiche_bsp_bis b;

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

	let s_d = Segment.new_segment 2 0 2 3 in
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

	Render.display b2 player2; 

	(*resultat attendu = 
		
		xa: 1, ya: -1
 		xb: 1, yb: -3

	*)

	let s_d3 = Segment.new_segment (-3) 1 (-1) 3 in
	let b3 = Bsp.build_bsp (s_d3::[]) in
	let player3 = Player.new_player (Point.new_point (-1) 1) 135 in

	Render.display b3 player3

	(*
	resultat attendu =

		xa: 1, ya: 1
		xb: 1, yb, -1
		attention normalement c'est pas ca 
		mais avec les histoires de int/float c'est moins précis

		Tout est ok
	*)

	
;;