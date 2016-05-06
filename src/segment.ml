open Point
open String
(* cree deux segment *)

type t = {id : string; 
          porig : Point.t; 
          pdest : Point.t;
          boite_gauche_orig : Point.t;
          boite_gauche_dest : Point.t;
          boite_droite_orig : Point.t;
          boite_droite_dest : Point.t;
         }

type tpos = L | R | C

let compteur =
	let etat = ref 0 in
	fun () -> etat := !etat + 1; !etat 

let angleNormal xa ya xb yb = 
	let y = float_of_int (yb - ya) in
	let x = float_of_int (xb - xa) in

	((atan (y /. x)) /. 3.14159 +. 1.) *. 10. (* calcule de l'angle de la normal et le borne entre 10 et -10*)
	

let new_segment xo yo xd yd = let ig = {x = xo ; y = yo} in 
	let est = {x = xd ; y = yd} in 
	let ang = truncate (angleNormal xo yo xd yd *. 10. )in
	let igG = {x = xo - ang ; y = yo - ang} in 
	let estG = {x = xd - ang ; y = yd - ang} in 
	let igD = {x = xo + ang ; y = yo + ang} in 
	let estD = {x = xd + ang ; y = yd + ang} in 
	let c = compteur() in 
	let p = {id = string_of_int c; porig = ig ; pdest = est;
			 boite_gauche_orig = igG; boite_gauche_dest = estG;
			 boite_droite_orig = igD; boite_droite_dest = estD} 
	in p

let dansLaBoite p s = 
	let res1 = (s.boite_gauche_dest.x - s.boite_gauche_orig.x) * (p.y - s.boite_gauche_orig.y) - 
	(s.boite_gauche_dest.y - s.boite_gauche_orig.y)* (p.x - s.boite_gauche_orig.x) in
	let res2 = (s.boite_droite_dest.x - s.boite_droite_orig.x) * (p.y - s.boite_droite_orig.y) - 
	(s.boite_droite_dest.y - s.boite_droite_orig.y)* (p.x - s.boite_droite_orig.x) in
	if res1 >= 0 && res2 <= 0 then raise Exit


let get_position p s = 
	let res = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y)* (p.x - s.porig.x) in
	match res with
	| a when a > 0 -> L
	| a when a < 0 -> R
	| _ -> C

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
	
	let xa = float_of_int s.porig.x in 
	let xb = float_of_int s.pdest.x in 
	let ya = float_of_int s.porig.y in 
	let yb = float_of_int s.pdest.y in 
	let xc = float_of_int d.porig.x in 
	let xd = float_of_int d.pdest.x in 
	let yc = float_of_int d.porig.y in 
	let yd = float_of_int d.pdest.y in 

	let dd = ((xb -. xa) *. (yd -. yc) -. (yb -. ya) *. (xd -. xc)) in

	let r = ((ya -. yc) *. (xd -. xc) -. (xa -. xc) *. (yd -. yc)) /. dd in 
	let xi = truncate (xa +. r *. (xb -. xa))  in 
    let yi = truncate (ya +. r *. (yb -. ya))  in 

    (Some (new_segment (truncate xa) (truncate ya) xi yi), Some(new_segment xi yi (truncate xb) (truncate yb)))	

let (+::) e l = match e with None -> l | Some e -> e :: l 


let split hd rest = 
	let rec split_terminal l (sl,sr) =
	match l with
	| [] -> (sl,sr)
	| a::b -> let (l,r) = split_segment hd a in
				split_terminal b (l+::sl, r+::sr) 
in
split_terminal rest ([],[])


