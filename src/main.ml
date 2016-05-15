open Test 
open Graphics
open Parse_lab
open Options
open Point
open Segment
open Player
open Bsp
open Render
open Random
open Ennemi
open Generateur
open Peintre


let ennemi = ref [Ennemi.new_ennemi (new_point 0 0)]

(*Fait des actions selon la touche pressée*)
exception Invalid_Touche
let deplacement p bsp = function
	|TZ -> Player.move MFwd p !bsp
    |TQ-> Player.move MLeft p !bsp
    |TS -> Player.move MBwd p !bsp
    |TD -> Player.move MRight p !bsp
	|TA -> Player.rotate Left p
	|TE -> Player.rotate Right p   
	|KKK -> raise Invalid_Touche
	|TC -> Player.courir p
	|TR -> Player.reset p
	|TF -> bsp := Player.tire p ennemi !bsp
	|TU -> Player.reset p; Render.go_solveur p (!Generateur.solution) !bsp
	|TB -> Peintre.couleur_prec p  
	|TN -> Peintre.couleur_suivante p 
	|TV -> Peintre.colorier p !bsp
    |_ -> Printf.printf "gnneeeeeuuuh je suis trizomique, je tape sur une mauvaise touche\n"

(*boucle principale du jeu*)
let rec jeu p bsp = 
	Render.display !bsp p;
	let mousePosX,_ = Graphics.mouse_pos () in
	let ev = wait_next_event [Key_pressed; Button_down; Mouse_motion] in
	if ev.keypressed then (match ev.key with
							|' ' -> jeu p bsp
							|'y' -> ()
							|a -> deplacement p bsp (Options.clavier_lg a); jeu p bsp)
	else if ev.button then ( bsp := Player.tire p ennemi !bsp ; jeu p bsp)
	else 
		(let dirAngle = mousePosX - ev.mouse_x in 
		 Player.rotate ~angle:(dirAngle) Left p;jeu p bsp)


let initialisation ((x, y, pa), lab) = 
	let rec ini sl = function
		|[] -> sl
		|(xa, ya, xb, yb)::s ->ini ((new_segment xa ya xb yb)::sl) s
	in
	let p = new_player (new_point x y) pa in
	p,ini [] lab	 

(*transforme des segments en portails en mode stargate oh yeah :) *)
let portail lab =
	
	let transformation = (1,10)::[] in 

	let rec p l t  =
		if not (t = []) then 
		begin
			match l with
			| [] -> ()
			| seg::b -> 
			let (deb,fin) = List.hd t in
			
			if int_of_string (seg.id) = deb then
			begin
				seg.id_autre <- fin;
				p b (List.tl t)
			end
			else
				p b t
		end
		else
			()
 	in

 	p lab transformation 

(*rajoute un mechant*)
let add_mechant bsp =
	let mechant = List.hd !ennemi in
	Bsp.add_ennemi mechant.ide mechant.position bsp

(*lance la fenetre graphique et donne la main à la boucle principale du jeu*)
let () = 

	Random.self_init ();

	Generateur.generateur (); 
	let p, lab = initialisation (Parse_lab.read_lab (  open_in Sys.argv.(1) )) in
	portail lab;
	let bsp = Bsp.build_bsp lab in
	let bsp = ref (add_mechant bsp) in
	let s = string_of_int (Render.taille) in 
	let a =  " " ^ s ^ "x" ^ s in


	Graphics.open_graph a;
	auto_synchronize false;
	Peintre.couleur_suivante p;
	jeu p bsp;
	Graphics.close_graph ()
(* 
let () = 
	let p, lab = (Parse_lab.read_lab (open_in Sys.argv.(1))) in
	Test.test (p, lab) *)