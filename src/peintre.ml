open Player
open Segment
open Bsp
open Render
open Trigo
open Point

let tableau = [|
 Graphics.white ;
 Graphics.red ;
 Graphics.green ;
 Graphics.blue ;
 Graphics.yellow ;
 Graphics.cyan ;
 Graphics.magenta 
 |]

let i = ref 0

let couleur_prec player =
	(if !i = 0 then i := (Array.length tableau) -1 else i:= !i - 1);
	player.color <- tableau.(!i)


let couleur_suivante player =
	(if !i = ((Array.length tableau)-1) then i := 0 else incr i);
	player.color <- tableau.(!i)

let colorier perso bsp =
	try
		Bsp.parse (fun s -> 

			let nw_seg = calcul_angle perso (calcul_vecteur perso s) in 
			let clip = clipping nw_seg in

			match clip with
			| None -> ()
			| Some(seg) -> 

				let point = Point.new_point (perso.pos.x + truncate(100.*. (dcos perso.pa)))
											(perso.pos.y + truncate(100.*. (dsin perso.pa)))
										in

				Printf.printf "joueur a %d %d joueur b %d %d \n segment orig %d %d dest %d %d \n"
				perso.pos.x perso.pos.y point.x point.y
				seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y ;

				let segment = Segment.new_segment (perso.pos.x)
												  (perso.pos.y)
												  (point.x)
												  (point.y)
												in


				match get_position seg.porig segment, get_position seg.pdest segment with
				| a,b when not (a = b) -> s.couleur <- perso.color ; raise Exit
				| _ -> ()
				) bsp perso.pos

		
	with Exit -> Render.display bsp perso