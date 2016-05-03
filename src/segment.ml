open Point
open String

type t = {id : string; 
          porig : Point.t; 
          pdest : Point.t;
         }

type tpos = L | R | C

let compteur () =
	let etat = ref 0 in
	fun () -> etat := !etat + 1; !etat 

let new_segment xo yo xd yd = let ig = {x = xo ; y = yo} in 
	let est = {x = xd ; y = yd} in 
	let c = compteur() in 
	let p = {id = string_of_int(c()); porig = ig ; pdest = est } 
	in p

let get_position p s = 
	let res = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y)* (p.x - s.porig.x) in
	match res with
	| a when a > 0 -> L
	| a when a < 0 -> R
	| _ -> C

let split_segment d s = 
	match get_position s.porig d, get_position s.pdest d with
	| L,L -> (Some (s), None)
	| R,C -> (None, Some (s)) 
	| R,R -> (None, Some (s)) 
	| C,R -> (None, Some (s)) 
	| L,C -> (Some (s), None)
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

	(*if dd = 0 then (Some (s), None)
	else*)
	(* Printf.printf "dd: %d" dd; *) 

	let r = ((ya -. yc) *. (xd -. xc) -. (xa -. xc) *. (yd -. yc)) /. dd in 
	let xi = truncate (xa +. r *. (xb -. xa))  in 
    let yi = truncate (ya +. r *. (yb -. ya))  in 
    (* Printf.printf "result: %f\n" ((ya -. yc) *. (xd -. xc) -. (xa -. xc) *. (yd -. yc));
    Printf.printf "dd: %f\n" dd;
    Printf.printf "r: %f\n" r;
    Printf.printf "xi: %d\n" xi;
    Printf.printf "yi: %d\n" yi; *)
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


