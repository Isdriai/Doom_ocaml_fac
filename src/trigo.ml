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