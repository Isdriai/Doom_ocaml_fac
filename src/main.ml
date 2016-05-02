open Options
open Point
open Segment
open Player
open Bsp

let () = 
	let p = Point.new_point 3 3 in 
	let player = Player.new_player p 80 in
	Player.rotate Left player;
	Printf.printf "test rotate de player, il était de 80 : %d \n" player.pa ;

	let s1 = Segment.new_segment 0 0 1 1 in 
	let s2 = Segment.new_segment 1 0 3 0 in 
	let s3 = Segment.new_segment 2 1 2 3 in 
	let s4 = Segment.new_segment 3 1 4 0 in 
	let l_seg = s1::s2::s3::s4::[] in
	let b = Bsp.build_bsp l_seg in

	(*point, player et segment marchent car on fait un build bsp sans probleme
	en effet, build bsp appelle les fonctions de segment*)
	(*a tester = parse, rev_parse et iter de bsp*)

	Printf.printf "ok\n"

;;

