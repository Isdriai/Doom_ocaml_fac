open Segment

type t = E | N of Segment.t * t * t 

let rec parse f bsp p = 
	match bsp with
	| E -> ()
	| N (r, g, d) -> if get_position p r = L then (parse f g p; f r; parse f d p)
					else (parse f d p; f r; parse f g p)

let rec rev_parse f bsp p = match bsp with
	| E -> ()
	| N (r, g, d) -> if not (get_position p r = L) then (rev_parse f g p; f r; rev_parse f d p)
					else (rev_parse f d p; f r; rev_parse f g p)


let rec iter f =  function 
	| E -> ()
	| N (r, g, d) -> iter f g ; f r; iter f d


let delta_min (x, ll, lr) (s, l, min) =
	let delta = abs (List.length ll - List.length lr) in
	if delta < min then (x , (ll, lr), delta)
	else (s, l, min) 

let rec equilibrage_bsp sl aVoir acc =
	match aVoir with
	|[] -> acc
	|x::s -> let ll,lr = split x (List.filter (fun e -> x != e) sl) in equilibrage_bsp sl s (delta_min (x, ll, lr) acc)

let build_bsp sl = 
	let rec b_bsp = function
		| []-> E
		| l -> let (s, (ll, lr), _) = equilibrage_bsp l l (List.hd sl, ([], []), 80000) in N (s, b_bsp ll, b_bsp lr)
		(* | x::s -> let (ll, lr) = split x s in N (x, b_bsp ll, b_bsp lr) *)
	in
	b_bsp sl



(* let rec iter_cps f bsp=  
	let rec iter cont = function 
	| E -> cont ()
	| N (r, g, d) -> let k  = (fun () -> f r; iter cont d)
					in iter k g
	in	
	iter (fun _ ->()) bsp


 
let rec parse_cps f bsp p = 
	let rec parse cont bsp =
		match bsp with
		| E -> cont ()
		| N (r, g, d) -> if get_position p r = L then (let k =  fun () -> f r; parse cont d in parse k g)
						else(let k =  fun () -> f r; parse cont g in parse k d)
	in
	parse (fun _ -> ()) bsp




let rec rev_parse_cps f bsp p = 
	let rec rev_parse cont bsp =
		match bsp with
		| E -> cont ()
		| N (r, g, d) -> if get_position p r = L then (let k =  fun () -> f r; rev_parse cont g in rev_parse k d)
						else(let k =  fun () -> f r; rev_parse cont d in rev_parse k g)
	in
	rev_parse (fun _ -> ()) bsp




(* 
let build_bsp_cps sl = 
	let rec b_bsp cont = function
		| []-> cont E
		| x::s -> let (ll, lr) = split x s in let k = fun n -> match n with | None -> Some (, E) | Some r g d ->  N(r, )
		in b_bsp k ll	
	in
	b_bsp (fun n -> match n with | E -> E | a -> a)
		sl *)

let build_bsp_cps sl = (*presque*)
	let rec b_bsp cont = function
		| []-> cont None
		| x::s -> let (ll, lr) = split x s in
				let k n = (match n with | None -> Some (E) | Some g ->  Some (N(x, g, let Some(d) = b_bsp cont lr in d))) in 
				b_bsp k ll	
	in
	b_bsp (fun n -> match n with | None -> Some E | a -> a)	sl *)