open Segment 

type t = E | N of Segment.t * t * t | Ennemi of (int * Point.t) list



let rec parse ?h:(b = fun _ -> ()) f bsp p = 
	match bsp with
	| Ennemi x -> List.iter b x 
	| E -> ()
	| N (r, g, d) -> if get_position p r = L then (parse f g p; f r; parse f d p)
					else (parse f d p; f r; parse f g p)

let rec rev_parse ?h:(b = fun _ -> ()) f bsp p  = 
	match bsp with
	| Ennemi x -> List.iter b x
	| E -> ()
	| N (r, g, d) -> if not (get_position p r = L) then (rev_parse f g p; f r; rev_parse f d p)
					else (rev_parse f d p; f r; rev_parse f g p)


let rec iter f =  function
	| Ennemi _ 
	| E -> ()
	| N (r, g, d) -> iter f g ; f r; iter f d


let delta_min (x, ll, lr) (s, l, min) =
	let delta = abs (List.length ll - List.length lr) in
	if delta < min then (x , (ll, lr), delta)
	else (s, l, min) 

let rec equilibrage_bsp sl aVoir acc =
	match aVoir with
	|[] -> acc
	|x::s -> let ll,lr = split x (List.filter (fun e -> x != e) sl) in 
			equilibrage_bsp sl s (delta_min (x, ll, lr) acc)

let build_bsp sl = 
	let rec b_bsp = function
		| []-> E
		| l -> let (s, (ll, lr), _) = equilibrage_bsp l l (List.hd sl, ([], []), 80000) in
									 N (s, b_bsp ll, b_bsp lr)
	in
	b_bsp sl

let rec remove_ennemi_list id x s = 
	if x = id then 
		match s with
		| [] -> E
		| _ -> Ennemi (s)
	else
		match s with
		| [] -> E
		| (x, _)::s -> remove_ennemi_list id x s

let rec remove_ennemi id pos = function
	| Ennemi ((x, _)::s)  -> remove_ennemi_list id x s
	| N (r, g, d) ->  if get_position pos r = L then N(r, remove_ennemi id pos g, d)
					else N(r, g, remove_ennemi id pos d)
	| E -> raise Exit

let rec add_ennemi id pos = function
	| E -> Ennemi [id,pos]
	| N (r, g, d) ->  if get_position pos r = L then N(r, add_ennemi id pos g, d)
					else N(r, g, add_ennemi id pos d)

	| Ennemi s -> Ennemi ((id, pos)::s)