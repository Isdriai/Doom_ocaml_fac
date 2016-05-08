let pi = 4. *. atan 1.

let piSur2 = pi /. 2.
let pidiv = pi /. 180.
let ipidiv = 180. /. pi

let d_to_rad a = float a *. pidiv
let r_to_deg a = a *. ipidiv

let rtan a = tan a
let dtan a = tan (d_to_rad a)

let dcos a = cos (d_to_rad a)

let dacos c = r_to_deg (acos c)

let dsin a = sin (d_to_rad a)

(* let point_intersection_droites s d =
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

    (xi,yi) *)