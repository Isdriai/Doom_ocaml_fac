open Options
open Point
open Segment
open Player
open Bsp

let affiche_segment s = 
	Printf.printf "xa: %d, ya: %d\n xb: %d, yb: %d\n" s.porig.x s.porig.y s.pdest.x s.pdest.y


let affiche_bsp bsp = 
	Bsp.iter affiche_segment bsp

let rec affiche_split (l,r) =
	match l,r with
	| [],[] -> ()
	| sl::g, sr::d -> affiche_segment sl; 
					  affiche_segment sr;
					  affiche_split (g,d)

	| [], sr::d -> affiche_segment sr; affiche_split ([], d)
	| sl::g , [] ->  affiche_segment sl; affiche_split (g, [])

let () = 
	let p = Point.new_point 3 3 in 
	let player = Player.new_player p 80 in
	Player.rotate Left player;

	let s1 = Segment.new_segment 0 0 1 1 in 
	let s2 = Segment.new_segment 1 0 3 0 in 
	let s3 = Segment.new_segment 2 1 2 3 in 
	let s4 = Segment.new_segment 3 1 4 0 in 
	let l_seg = s1::s2::s3::s4::[] in
	let test_split = Segment.split s1 l_seg in 
	let b = Bsp.build_bsp l_seg in
	Printf.printf "bsp :\n\n";
	affiche_bsp b;
	Printf.printf "affichage split :\n\n";
	affiche_split test_split

;;
	(*point, player et segment marchent car on fait un build bsp sans probleme
	en effet, build bsp appelle les fonctions de segment*)
	(*a tester = parse, rev_parse et iter de bsp*)




