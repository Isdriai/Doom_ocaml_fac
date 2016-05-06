open Test 
open Graphics
open Parse_lab
open Options
open Point
open Segment
open Player
open Bsp
open Render


(* let () = 
	let lab = Parse_lab.read_lab (open_in Sys.argv.(1)) in
	test lab;
	let s = Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed]
	 in ()

;; *)

exception Invalid_Touche
let deplacement p bsp = function
	|TZ -> Player.move MFwd p bsp
    |TQ-> Player.move MLeft p bsp
    |TS -> Player.move MBwd p bsp
    |TD -> Player.move MRight p bsp
	|TA -> Player.rotate Left p
	|TE -> Player.rotate Right p   
    |_ -> raise Invalid_Touche

let rec jeu p bsp = 
	Render.display bsp p;
	match (wait_next_event [Key_pressed]).key with
	|' ' -> jeu p bsp
	|'y' -> ()
	|a -> deplacement p bsp (Options.clavier_lg a); jeu p bsp

let initialisation ((x, y, pa), lab) = 
	let rec ini sl = function
		|[] -> sl
		|(xa, ya, xb, yb)::s ->ini ((new_segment xa ya xb yb)::sl) s
	in
	let p = new_player (new_point x y) pa in
	p,ini [] lab	 


let () = 
	let p, lab = initialisation (Parse_lab.read_lab (open_in Sys.argv.(1))) in
	let bsp = Bsp.build_bsp lab in
	let s = string_of_int (Render.taille) in 
	let a =  " " ^ s ^ "x" ^ s in

	Graphics.open_graph a;
	auto_synchronize false;
	jeu p bsp;
	Graphics.close_graph ()