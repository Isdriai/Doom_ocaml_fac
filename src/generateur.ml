open Random
open Options
open Player 
open Point

type case = {
	id : int;
	mutable fait : bool;
	mutable mur_droite : bool;
	mutable mur_haut : bool;
}

let taille = 6

let longueur = 300

let (+::) e l = match e with 
				| None -> l 
				| Some(elem) -> elem :: l 


let parcouru = Array.init (taille*taille) 
	(fun i -> let c = { id = i; fait = false ; mur_droite = true ; mur_haut = true; } in c)  

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


let solution = ref [] 

let parcourt (x_dep, y_dep) (x_fin, y_fin) =

	let murs_faits = ref 0 in

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

			if (x_b < x_a ) then verticaux.(x_b).(y_b) <- false
			else if ( y_a < y_b ) then horizontaux.(y_a).(x_a) <- false
			else if ( x_b > x_a ) then verticaux.(x_a).(y_a) <- false
		    else horizontaux.(y_b).(x_b) <- false  

	in

	let changement_etat_case (x,y) =
		parcouru.(y*taille + x).fait <- true
	in

	let rec par liste possibles =

		match liste with
		| (x_a,y_a::pile) -> 

		if not (!murs_faits = taille*taille -1) then
			match possibles with
			| a::b -> let suite = choix possibles in
			effacer_mur (x_a, y_a) suite;
			incr murs_faits;
			changement_etat_case suite;
			(if (x_fin, y_fin) = (x_a, y_a) then solution := ((x_a,y_a)::pile));
			par (suite::((x_a,y_a)::pile)) (possibilites suite)
			
			| [] -> let etat_precedent = List.hd pile in
					let pos_prec, possibles_prec = etat_precedent in
					par ((pos_prec, (possibilites pos_prec))::(List.tl pile)) 

		else
			(if !solution = [] then solution := ((x_fin,y_fin)::pile)
			 else ())
		| _ -> ()
 
		
	in

	changement_etat_case (0,0);
	par ((0,0)::[])

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

		let parcour_ecriture fonction mur =
		Array.iteri (
			fun index el -> 
				let deb = ref 0 in 

				for i = 0 to (taille-1) do 

 					if el.(i) then 
						if i = (taille-1) then begin
 							fonction index (!deb) (i+1);
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



let solveur player liste =
	match liste with
	| (x,y)::b -> begin (* Printf.printf " x %d y %d \n " x y ; *) player.pos <- Point.new_point ((x*longueur)+(longueur/2)) ((y*longueur)+(longueur/2)) ; b end
	| [] -> []