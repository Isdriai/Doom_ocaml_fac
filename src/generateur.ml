open Random
open Options
open Player 
open Point

let taille = 10

let longueur = 300

let (+::) e l = match e with 
				| None -> l 
				| Some(elem) -> elem :: l 


let parcouru = Array.init (taille*taille) 
	(fun _ -> false)  

let solution = ref [] 

let verticaux = Array.init (taille-1)
	(fun i -> Array.init (taille)
		(fun j -> true
		)
	)

let horizontaux = Array.init (taille-1)
	(fun i -> Array.init (taille)
		(fun j -> true
		)
	)

(*description de l'algo, on commence a une case, on choisit une case non visitée et on détruit le mur les separant
si on a plus de choix on remonte la pile d'appel jusqu'a trouver une case ou il y encore un choix

on fait ca jusqu'à avoir retirer taille²-1 murs ( propriétés des labyrinthes parfaits )
*)

let parcourt (x_dep, y_dep) (x_fin, y_fin) =

	let murs_faits = ref 0 in

	let possibilites (x,y) =
		let eins = 
			if (y > 0) then
				if (parcouru.(taille*(y-1) +x )) then None
				else Some(x, (y-1))
			else None

		in

		let zwei = 
			try
				if (parcouru.(taille*(y+1) +x )) then None
				else Some(x, (y+1))
			with _ -> None
		in
		let drei = 
			if ( x > 0) then
				if (parcouru.(taille*y +(x-1))) then None
				else Some(x-1, y)
			else None
		in
		let vier = 
			if ( x < taille-1) then
				if (parcouru.(taille*y +(x+1) )) then None
				else Some(x+1, y)
			else None
		in

		let pos = (eins+::(zwei+::(drei+::(vier+::[])))) in 
		pos

	in

	let choix liste =
		Random.self_init ();

		let nbr = Random.int (List.length liste) in

		let rec ch l n =
			match l with
			| a::b::c -> if n=0 then a else ch (b::c) (n-1) 			
			| a::b -> a (*b est forcement la liste vide*)
			| [] -> (0,0) (*n'arrive jamais*)
		in

		ch liste nbr

	in

	let effacer_mur (x_a, y_a) (x_b, y_b) = 

			if (x_b < x_a ) then verticaux.(x_b).(y_b) <- false
			else if ( y_a < y_b ) then horizontaux.(y_a).(x_a) <- false
			else if ( x_b > x_a ) then verticaux.(x_a).(y_a) <- false
		    else horizontaux.(y_b).(x_b) <- false  

	in

	let changement_etat_case (x,y) =
		parcouru.(y*taille + x) <- true
	in

	let rec par liste possibles =

		match liste with
		| ((x_a,y_a)::pile) -> 

		if not (!murs_faits = taille*taille -1) then
			match possibles with
			| a::b -> let suite = choix possibles in
			effacer_mur (x_a, y_a) suite;
			incr murs_faits;
			changement_etat_case suite;
			(if (x_fin, y_fin) = (x_a, y_a) then solution := (List.rev_append ((x_a,y_a)::pile)) []);
			par (suite::liste) (possibilites suite)
			
			| [] -> let etat_precedent = List.hd pile in
					par (etat_precedent::(List.tl pile)) (possibilites etat_precedent)

		else
			(if !solution = [] then solution := ((x_fin,y_fin)::pile)
			 else ())
		| _ -> ()
 
		
	in

	changement_etat_case (0,0);
	par ((0,0)::[]) (possibilites (0,0))

let ecrire_fichier () =
	let fichier = open_out Options.nom_lab in

	let ecrire_perso () =
		let emplacement = longueur/2 in
		output_string fichier  ("P : "^ (string_of_int (emplacement)) ^" " ^(string_of_int emplacement) ^ " 90\n")
	in

	let ecrire_contour ()=
			let decal = (string_of_int (taille*longueur)) in
			output_string fichier  ("0 0 0 " ^ decal ^ "\n");
			output_string fichier  ("0 0 "^ decal ^" 0\n");
			output_string fichier  ("0 " ^ decal ^ " " ^ decal ^ " " ^ decal ^ "\n");
			output_string fichier  (decal ^ " " ^ decal ^ " " ^ decal ^ " 0" ^ "\n")
		in

	let ecrire_murs () =

		let ecrire_mur xa ya xb yb =
			 output_string fichier (
									(string_of_int(xa)) ^ " " ^
									(string_of_int(ya)) ^ " " ^
									(string_of_int(xb)) ^ " " ^
									(string_of_int(yb)) ^ "\n"
									) 
		in 

		(*si deux murs sont collés, la fonction n'en fera qu'un seul qui sera la fusion des deux anciens
		
		Pour le faire, on enregistre la derniere fois qu'on a rencontré un mur et on regarde quand on tombe sur un trou

		 *)

		let parcour_ecriture fonction mur =
		Array.iteri (
			fun index el -> 
				let deb = ref 0 in 

				for i = 0 to (taille-1) do 

 					if el.(i) then 
						if i = (taille-1) then begin
 							fonction index (!deb) (i+1); (*cas particulier ou on est à la fin*)
						end
						else 
							()
					else
						if i = !deb then 
							incr deb
						else 
							begin
 							fonction index (!deb) i;
							deb := i+1
						end
				done
			) mur
		in 
		
  		parcour_ecriture (fun index i deb -> ecrire_mur ((index+1)*longueur) (deb*longueur) ((index+1)*longueur) (i*longueur)) verticaux;
 		  
		parcour_ecriture (fun index i deb -> ecrire_mur (deb*longueur) ((index+1)*longueur) (i*longueur) ((index+1)*longueur)) horizontaux
 	in

	ecrire_perso ();
	ecrire_contour ();
	ecrire_murs();

	close_out fichier



(*lis la solution du labyrinthes précedemment écrite dans un fichier*)
let lecture_solution () =
		let fichier = open_in Options.nom_solution in 

		let recupere_paire str =

			let x = ref "" in
			let y = ref "" in
			let ok = ref false in 

			(for i = 0 to ((String.length str)-1) do 
				
				if !ok then 
					y := !y ^ (Char.escaped (str.[i]))
				else if (str.[i] = '\ ') then 
					ok := true
				else 
					x := !x ^ (Char.escaped (str.[i]))

			done);

			(int_of_string !x, int_of_string !y)
		in
		
		let rec lecture solution =
			try
				let str = input_line fichier in 
				lecture (recupere_paire str::solution)
			with
			| End_of_file -> List.rev_append solution []  
		in

		solution := lecture []

let ecrire_solution liste =

	let fichier = open_out Options.nom_solution in
	let rec e_s l =
		match l with
		| (a,c)::b -> begin 

			let s = ((string_of_int a) ^ " " ^ (string_of_int c) ^ "\n") in 
			output_string fichier s; 
			e_s b end

		| [] -> ()
	in

	e_s liste 


let generateur () =
	parcourt (0,0) (taille-1,taille-1);
	ecrire_solution !solution;
	ecrire_fichier ()

(*Repositionne le joueur sur la case finale grace à la solution chargée depuis un fichier*)
let solveur player liste =
	match liste with
	| (x,y)::b -> begin (* Printf.printf " x %d y %d \n " x y ; *) player.pos <- Point.new_point ((x*longueur)+(longueur/2)) ((y*longueur)+(longueur/2)) ; b end
	| [] -> []