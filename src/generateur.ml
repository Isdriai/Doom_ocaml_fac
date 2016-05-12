open Random
open Options

type case = {
	id : int;
	mutable fait : bool;
	mutable mur_droite : bool;
	mutable mur_haut : bool;
}

let taille = 3

let longueur = 50 

let (+::) e l = match e with 
				| None -> l 
				| Some(elem) -> elem :: l 

let parcouru = Array.init (taille*taille) 
	(fun i -> let c = { id = i; fait = false ; mur_droite = true ; mur_haut = true; } in c)  

let solution = ref [] 

let parcourt (x_dep, y_dep) (x_fin, y_fin) =

	let murs_faits = ref 0 in


	(* Regarde les voisins d'une case et renvoie la liste de ceux qui sont visitables *)

	let possibilites (x,y) =
		let eins = 
			if (y > 0) then
				if (parcouru.(taille*(y-1) +x ).fait) then None
				else Some(x, (y-1))
			else None

		in

		let zwei = 
			try
				if (parcouru.(taille*(y+1) +x ).fait) then None
				else Some(x, (y+1))
			with _ -> None
		in
		let drei = 
			if ( x > 0) then
				if (parcouru.(taille*y +(x-1)).fait) then None
				else Some(x-1, y)
			else None
		in
		let vier = 
			if ( x < taille-1) then
				if (parcouru.(taille*y +(x+1) ).fait) then None
				else Some(x+1, y)
			else None
		in

		let pos = (eins+::(zwei+::(drei+::(vier+::[])))) in 
		pos

	in

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

			if (x_b < x_a) then  parcouru.(y_b*taille + x_b).mur_droite <- false 
			else if ( y_a < y_b ) then  parcouru.(y_a*taille + x_a).mur_haut <- false 
			else if (x_b > x_a ) then parcouru.(y_a*taille + x_a).mur_droite <- false 
			else parcouru.(y_b*taille + x_b).mur_haut <- false 

	in

	let extraction liste =
		let rec e l acc =
			match l with
			| ((x,y),_)::b -> e b ((x,y)::acc)
			| [] -> acc
		in
		e liste []
	in

	let changement_etat_case (x,y) =
		parcouru.(y*taille + x).fait <- true
	in

	let rec par (((x_a,y_a),(possibles))::pile) =

		if not (!murs_faits = taille*taille -1) then
			match possibles with
			| a::b -> let (suite, reste) = choix possibles in
			effacer_mur (x_a, y_a) suite;
			incr murs_faits;
			changement_etat_case suite;
			(if (x_fin, y_fin) = (x_a, y_a) then solution := extraction(((x_a,y_a),(possibles))::pile));
			par ((suite, possibilites suite)::(((x_a,y_a),reste)::pile))
			
			| [] -> let etat_precedent = List.tl pile in
					par etat_precedent
			
		else
			()
	in

	changement_etat_case (0,0);
	par (((0,0), possibilites (0,0))::[])





let ecrire_fichier () =
	let fichier = open_out Options.nom_lab in

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

	let rec affiche_liste l = 
		match l with
		| (a,c)::b -> Printf.printf "         x %d y %d\n" a c; affiche_liste b
		| [] -> ()
		
(* let affiche () =
	Array.iter (
		fun i -> Printf.printf "id %d fait %b mur_droite %b          mur_haut %b\n" i.id i.fait i.mur_droite i.mur_haut
	)
	parcouru *)

let generateur () =
	parcourt (0,0) (taille-1,taille-1);
	affiche_liste !solution;
(* 	affiche ();
 *)	ecrire_fichier ()
