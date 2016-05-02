type t = {
  id : string;
  porig : Point.t; 
  pdest : Point.t;
}

type tpos = L | R | C

val new_segment : int -> int -> int -> int -> t

val get_position : Point.t -> t -> tpos

val split_segment : t -> t -> t option * t option

val split : t -> t list -> t list * t list
