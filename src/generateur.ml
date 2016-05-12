open Random
open Options

type case = {
	id : int;
	mutable fait : bool;
	mutable mur_droite : bool;
	mutable mur_haut : bool;
}


let taille = 2

let (+::) e l = match e with None -> l | Some e -> e :: l 

let parcouru = Array.init (taille*taille) 
	(fun i -> let c = { id = i; fait = false ; mur_droite = true ; mur_haut = true; } in c)  


let parcourt (x_dep, y_dep) (x_fin, y_fin) =

	let murs_faits = ref 0 in

	let solution = ref [] in

	(* Regarde les voisins d'une case et renvoie la liste de ceux qui sont visitables *)

	let possibilites x y =
		let eins = 
			try
				if parcouru.(taille*(y-1) +x ).fait then None
				else Some(x, (y-1))
			with _ -> None
		in

		let zwei = 
			try
				if parcouru.(taille*(y+1) +x ).fait then None
				else Some(x, (y+1))
			with _ -> None
		in
		let drei = 
			try
				if parcouru.(taille*y +(x-1)).fait then None
				else Some(x-1, y)
			with _ -> None
		in
		let vier = 
			try
				if parcouru.(taille*y +(x+1) ).fait then None
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

	let effacer_mur (x_a, y_a) (x_b, y_b) = 
		let (x_c, y_c) = 
			if (x_b > x_a) || ( y_b < y_a ) then  (x_b, y_b)
			else (x_a, y_a)
		in
		parcouru.(x_c+ y_c*taille).mur_haut <- false;
		parcouru.(x_c+ y_c*taille).mur_droite <- false;
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
		parcouru.(x_a+ y_a*taille).fait <- true;
		if not (!murs_faits = (taille*taille -1)) then (* nm -1*)
			match (possibilites x_a y_a)  with
			| a::b -> let (suite, autre_poss) = choix (a::b) in 
					effacer_mur (x_a, y_a) suite;
					incr murs_faits;
					(if (suite = (x_fin, y_fin)) then solution := chemin else ());
					par suite (((x_a, y_a)::chemin),(autre_poss::pos)) (((x_a, y_a),suite)::deplacements)
			| [] -> par (List.hd chemin) ((List.tl chemin),List.tl pos) deplacements
		else
			()
	in
	parcouru.(x_dep+ y_dep*taille).fait <- true;
	par (x_dep,y_dep) ([],[]) [];
	!solution


let ecrire_fichier () =
	let fichier = open_out Options.nom_lab in
	let longueur = 50 in

	let ecrire_perso () =
		let emplacement = longueur/2 in
		output_string fichier  ("P : "^ (string_of_int (emplacement+longueur)) ^" " ^(string_of_int emplacement) ^ " 90\n")
	in

	let ecrire_contour ()=
			output_string fichier  ("0 0 0 " ^ (string_of_int (taille*longueur)) ^ "\n");
			output_string fichier  ("0 0 "^ (string_of_int (taille*longueur)) ^" 0\n")
	in

	let ecrire_mur () =
		Array.iter 
		( fun i ->
			let x = i.id / taille in
			let y = i.id mod taille in

			(if i.mur_haut then 
							output_string fichier  (
											  (string_of_int (x*longueur)) ^ " " ^
											  (string_of_int ((y+1)*longueur))	^ " " ^
											  (string_of_int ((x+1)*longueur)) ^ " " ^
											  (string_of_int ((y+1)*longueur)) ^ "\n"
											)
						else ());

			(if i.mur_droite then 
							output_string fichier  (
								(string_of_int ((x+1)*longueur)) ^ " " ^
								(string_of_int (y*longueur)) ^ " " ^
								(string_of_int ((x+1)*longueur)) ^ " " ^
								(string_of_int ((y+1)*longueur)) ^ "\n"
							)
						else ())
		)
		parcouru
	in

	ecrire_perso ();
	ecrire_contour ();
	ecrire_mur();

	close_out fichier
	

let generateur () =
	let solution = parcourt (0,0) (taille,taille) in 
	ecrire_fichier ()
