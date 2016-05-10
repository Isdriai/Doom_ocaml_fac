open Point

let pi = 4. *. atan 1.

let piSur2 = pi /. 2.
let pidiv = pi /. 180.
let ipidiv = 180. /. pi

let bool_of_int b = 
	if b then 1 else 0
	
let d_to_rad a = float a *. pidiv
let r_to_deg a = a *. ipidiv

let rtan a = tan a
let dtan a = tan (d_to_rad a)

let dcos a = cos (d_to_rad a)

let dacos c = r_to_deg (acos c)

let dsin a = sin (d_to_rad a)

(*Pour éviter de mettre des segments et ainsi éviter des conflits dans la compilation,
 on demande 8 coordonnées des 2 droites*)

let point_intersection_droites xa_i ya_i xb_i yb_i xc_i yc_i xd_i yd_i =

	let xa = float_of_int xa_i in 
	let xb = float_of_int xb_i in 
	let ya = float_of_int ya_i in 
	let yb = float_of_int yb_i in 
	let xc = float_of_int xc_i in 
	let xd = float_of_int xd_i in 
	let yc = float_of_int yc_i in 
	let yd = float_of_int yd_i in 

	let dd = ((xb -. xa) *. (yd -. yc) -. (yb -. ya) *. (xd -. xc)) in

	let r = ((ya -. yc) *. (xd -. xc) -. (xa -. xc) *. (yd -. yc)) /. dd in 
	let xi = truncate (xa +. r *. (xb -. xa))  in 
    let yi = truncate (ya +. r *. (yb -. ya))  in 

    (xi,yi) 

let points_intersection_droite_cercle a b dist_limite =

(*C'est juste pour éviter une division par 0, et on va estimer que l'echelle globale 
de la carte est beaucoup plus grande que cette petite rectification *)

	Printf.printf "pidc xa: %d, ya: %d    xb: %d, yb: %d \n" a.x a.y b.x b.y;


	let triche = ref 0 in

(* 	(if a.x = b.x then triche := 1 else () ); *)

	let coef =  (float_of_int(b.y - a.y ) /. float_of_int(b.x + !triche - a.x)) in
	let ordonnee = float_of_int b.y -. (coef *. float_of_int b.x ) in
	Printf.printf "coef = %f ; ordonnee = %f \n" coef ordonnee;
	Printf.printf "b.y %d a.y %d b.x %d a.x %d\n" b.y a.y b.x a.x;


	let d = 1. +. coef in
	let e = 2. *. coef *. ordonnee in 
	let f = (ordonnee **2.) -. dist_limite**2. in



	let delta = (e**2.) -. (4. *. d *. f) in
	Printf.printf "A = %f\nB= %f\nC= %f\ndelta = %d\n" d e f (int_of_float delta);
	if delta > 0. then

		let x1 = (-.e -. (sqrt delta)) /. (2. *. d) in
		let x2 = (-.e +. (sqrt delta)) /. (2. *. d) in
		Some(int_of_float x1),
		Some(int_of_float (coef *. x1 +. ordonnee)),
		Some(int_of_float x2),
		Some(int_of_float (coef *. x2 +. ordonnee))

	else 
		None,None,None,None