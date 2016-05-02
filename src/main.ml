open Options
open Point
open Segment
open Player
open Bsp

let affiche_point p = 
	Printf.printf "x: %d, y: %d\n" p.x p.y

let affiche_segment s = 
	Printf.printf "xa: %d, ya: %d\n xb: %d, yb: %d\n" s.porig.x s.porig.y s.pdest.x s.pdest.y


let affiche_bsp bsp = 
	Bsp.iter affiche_segment bsp

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
	let s1 = Segment.new_segment 0 0 1 1 in 
	let s2 = Segment.new_segment 1 0 3 0 in 
	let s3 = Segment.new_segment 2 1 2 3 in 
	let s4 = Segment.new_segment 3 1 4 0 in 
	let l_seg = s1::s2::s3::s4::[] in
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

	(*on met le joueur à 90° pr tester ds un cas facile au début*)

	Player.rotate Player.Left player;
	Player.move Player.MFwd player (Bsp.build_bsp []);
	affiche_point player.pos;

	(*on test avec un léger decalage*)

	Player.rotate Left player;
	Player.move Player.MFwd player (Bsp.build_bsp []);
	affiche_point player.pos;

	(*on voit bien que ca marche, voir la sortie, 
	on remet le joueur comme avant*)

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
	affiche_point player.pos

	(*on voit que tout marche *)



;;




