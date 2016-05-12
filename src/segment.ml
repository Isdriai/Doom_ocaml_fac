open Point
open Trigo
open Options
open Random

type t = {id : string; 
          porig : Point.t; 
          pdest : Point.t;
          boite_gauche_orig : Point.t; (* boite de collision*)
          boite_gauche_dest : Point.t;
          boite_droite_orig : Point.t;
          boite_droite_dest : Point.t;
          mutable id_autre : int;
         }

type tpos = L | R | C

let compteur =
	let etat = ref 0 in
	fun () -> etat := !etat + 1; !etat 

let angleNormal xa ya xb yb = 
	let y = float_of_int (yb - ya) in
	let x = float_of_int (xb - xa) in
	if x = 0. then Trigo.piSur2
	else
	atan (y /. x) (* calcule de l'angle de la normal*)
	

let new_segment ?(s = 0) xo yo xd yd = 
	let pas = Options.step_dist /. 2. in
	let ig = {x = xo ; y = yo} in 
	let est = {x = xd ; y = yd} in 
	let ang = angleNormal xo yo xd yd in
	let igG = {x = xo - (int_of_float (cos (ang +. Trigo.piSur2) *. pas)) ; 
				y = yo - (int_of_float (sin (ang +. Trigo.piSur2) *. pas))} in 
	let estG = {x = xd - (int_of_float (cos (ang +. Trigo.piSur2) *. pas)) ; 
				y = yd - (int_of_float (sin (ang +. Trigo.piSur2) *. pas))} in 
	let igD = {x = xo + (int_of_float (cos (ang +. Trigo.piSur2) *. pas)) ; 
				y = yo + (int_of_float (sin (ang +. Trigo.piSur2) *. pas))} in 
	let estD = {x = xd + (int_of_float (cos (ang +. Trigo.piSur2) *. pas)) ; 
				y = yd + (int_of_float (sin (ang +. Trigo.piSur2) *. pas))} in 
	let c = compteur() in 
	let p = {id = string_of_int c; porig = ig ; pdest = est;
			 boite_gauche_orig = igG; boite_gauche_dest = estG;
			 boite_droite_orig = igD; boite_droite_dest = estD;
			 id_autre = s
			 } 
	in 
	p

let dansLaBoite p s = 
	let res1 = (s.boite_gauche_dest.x - s.boite_gauche_orig.x) * (p.y - s.boite_gauche_orig.y) - 
	(s.boite_gauche_dest.y - s.boite_gauche_orig.y)* (p.x - s.boite_gauche_orig.x) in

	let res2 = (s.boite_droite_dest.x - s.boite_droite_orig.x) * (p.y - s.boite_droite_orig.y) - 
	(s.boite_droite_dest.y - s.boite_droite_orig.y)* (p.x - s.boite_droite_orig.x) in

	let res3 = (s.boite_droite_orig.x - s.boite_gauche_orig.x) * (p.y - s.boite_gauche_orig.y) - 
	(s.boite_droite_orig.y - s.boite_gauche_orig.y)* (p.x - s.boite_gauche_orig.x) in

	let res4 = (s.boite_droite_dest.x - s.boite_gauche_dest.x) * (p.y - s.boite_gauche_dest.y) - 
	(s.boite_droite_dest.y - s.boite_gauche_dest.y)* (p.x - s.boite_gauche_dest.x) in
	
	if res1 * res2 <= 0  && res3 * res4 <= 0 then raise Exit

(*Dis si un segment est à gauche, à droite ou au centre par rapport à un point*)
let get_position p s = 
	let res = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y)* (p.x - s.porig.x) in
	match res with
	| a when a > 0 -> L
	| a when a < 0 -> R
	| _ -> C

(*Sépare un segment s si il se fait couper par le prolongement d'un autre segment d*)
let split_segment d s = 
	match get_position s.porig d, get_position s.pdest d with
	| R,C
	| R,R
	| C,R -> (None, Some (s)) 
	| C,C
	| L,L 
	| L,C 
	| C,L -> (Some (s), None)
	| _,_ ->  

    let (xi,yi) = Trigo.point_intersection_droites s.porig.x s.porig.y s.pdest.x s.pdest.y d.porig.x d.porig.y d.pdest.x d.pdest.y in 
    let s1 = new_segment ~s:s.id_autre s.porig.x s.porig.y xi yi in 
    let s2 = new_segment ~s:s.id_autre xi yi s.pdest.x s.pdest.y in 

    let i_dont_know = get_position (s1.porig) d in
    match i_dont_know with
    | C
    | L -> (Some(s1),Some(s2))
    | R -> (Some(s2),Some(s1))

let (+::) e l = match e with None -> l | Some e -> e :: l 

(*renvoie la liste des segments à gauche et à droite par rapport à un segment hd,
 Si un segment se trouve au milieu, un segment correspondant à sa partie gauche sera mis avec les segments à gauche,
 de meme si pour la partie droite
*)
let split hd rest = 
	let rec split_terminal l (sl,sr) =
	match l with
	| [] -> (sl,sr)
	| a::b -> let (l,r) = split_segment hd a in
				split_terminal b (l+::sl, r+::sr) 
in
split_terminal rest ([],[])