open Player
open Segment
open Bsp
open Render

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
		Bsp.parse (fun s -> s.couleur <- perso.color ; raise Exit) bsp perso.pos
	with Exit -> Render.display bsp perso