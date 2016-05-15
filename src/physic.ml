open Bsp
open Segment

let recherche_segment id bsp =
	let tmp = ref (Segment.new_segment 0 0 0 0) in
	let fonction s =
		if int_of_string s.id = id then
		begin
			tmp:= s;
			raise Exit
		end
		else 
			()
	in

	(try 
  		Bsp.iter fonction bsp
  	with Exit -> ());

  	!tmp (*peut etre source de bug si l'id rentré 
  			ne correspond à aucun segment*)

let detect_collision p bsp =

	let seg = ref (Segment.new_segment 0 0 0 0) in

(* comme parse mais avec un segment qui est enregistré et renvoyer si collision *)
	let rec dc f bsp = 
		match bsp with
		| E -> ()
		| Ennemi(_) -> ()
		| N (r, g, d) -> seg := r;
						if get_position p r = L then (dc f g; f r;dc f d)
						else (dc f d; f r;dc f g)
	in
	try 
		dc (Segment.dansLaBoite p) bsp; (false, None)
	with Collision -> 
		if (!seg).id_autre = 0 then
			(true, None)
		else
			(true, Some(recherche_segment (!seg).id_autre bsp))