type t = {id : string; 
          porig : Point.t; 
          pdest : Point.t;
          boite_gauche_orig : Point.t;
          boite_gauche_dest : Point.t;
          boite_droite_orig : Point.t;
          boite_droite_dest : Point.t;
          id_autre : int;
         }


type tpos = L | R | C

val dansLaBoite : Point.t -> t -> unit

val new_segment : ?s:(int) -> int -> int -> int -> int -> t

val get_position : Point.t -> t -> tpos

val split_segment : t -> t -> t option * t option

val split : t -> t list -> t list * t list