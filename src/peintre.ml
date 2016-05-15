open Player
open Segment
open Bsp
open Render
open Trigo
open Point
open Generateur
open Random

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
		Bsp.parse (

				(*!!!!! ATTENTION LE PEINTRE DEVIENT FOU !!!!!*)

				fun s ->
				let chance = Random.int 5 in
				if chance = 0 then s.couleur <- perso.color else ()

				) bsp perso.pos

		
	with Exit -> Render.display bsp perso