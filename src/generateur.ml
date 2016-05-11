open Random

let taille = 100

let (+::) e l = match e with None -> l | Some e -> e :: l 

let parcourt (x_dep, y_dep) =

	let parcouru = Array.make (taille*taille) false in


	(* Regarde les voisins d'une case et renvoie la liste de ceux qui sont visitables *)

	let possibilites x y =
		let eins = 
			try
				if parcouru.(taille*(y-1) +x ) then None
				else Some(x, (y-1))
			with _ -> None
		in

		let zwei = 
			try
				if parcouru.(taille*(y+1) +x ) then None
				else Some(x, (y+1))
			with _ -> None
		in
		let drei = 
			try
				if parcouru.(taille*y +(x-1)) then None
				else Some(x-1, y)
			with _ -> None
		in
		let vier = 
			try
				if parcouru.(taille*y +(x+1) ) then None
				else Some(x+1, y)
			with _ -> None
		in

		eins+::(zwei+::(drei+::(vier+::[])))

	in


	(* 
		recoit une liste de possibilites et renvoie une paire,
		la premiere partie est le choix choisi 
		et la deuxieme partie est la liste d'origine retranchée du choix pris

		( int * int )list -> (int * int ) * (( int * int ) * list )

	*)

	let choix liste =
		Random.self_init ();

		let nbr = Random.int (List.length liste) in

		let rec ch l n avant=
			match l with
			| a::b::c -> if n=0 then (a,List.tl l) else ch (b::c) (n-1) (a::avant) 
			(*c'est pas grave si on change le sens des elements car on veut en prendre un aleatoirement*)
			
			| a::b -> (a,avant) (*b est forcement la liste vide*)
			| [] -> ((0,0),[]) (*n'arrive jamais*)
		in

		ch liste nbr []

	in

	let tout_fait () =
		try 
			Array.iter ( fun i -> if not i then raise Exit else () ) parcouru;
			true
		with Exit -> false
	in

	(*xa et ya sont les coordonnées de la position actuel,
	le deuxieme argument est un couple de listes,

	la premierre liste represente le chemin par ou on est passé,
	la deuxieme lsite est la liste qui correspon aux autres possibilités qu'on a eu

	le type du second arguemnt est 

	(int * int ) list * (( int * int ) list )  list

	le troisieme argument est le résumé des deplacements

	(point A -> point B) list

	avec point = int * int

	*)

	let rec par (x_a, y_a) (chemin, pos) deplacements =
		parcouru.(x_a+ y_a*taille) <- true;
		if not (tout_fait ()) then
			match (possibilites x_a y_a)  with
			| a::b -> let (suite, autre_poss) = choix (a::b) in 
					par suite (((x_a, y_a)::chemin),(autre_poss::pos)) (((x_a, y_a),suite)::deplacements)
			| [] -> par (List.hd chemin) ((List.tl chemin),List.tl pos) deplacements
		else
			deplacements
	in
	parcouru.(x_dep+ y_dep*taille) <- true;
	par (x_dep,y_dep) ([],[]) []




let ecrire_fichier murs_manquants =
	()

let generateur () =
	let murs_casses = parcourt (0,0) in 
	ecrire_fichier murs_casses
