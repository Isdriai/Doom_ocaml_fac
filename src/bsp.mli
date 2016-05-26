type t = E | N of Segment.t * t * t | Ennemi of (int * Point.t) list

val parse : ?h:((int * Point.t) -> unit) -> (Segment.t -> 'a) -> t -> Point.t -> unit

val rev_parse : ?h:((int * Point.t) -> unit) -> (Segment.t -> 'a) -> t -> Point.t -> unit

val iter : (Segment.t -> 'a) -> t -> unit

val build_bsp : Segment.t list -> t

val remove_ennemi : int -> Point.t -> t -> t

(*il faudra que add_ennemi verifie que l'ennemi est bien
a une certaine distance d'un mur,
cete distance se trouve dans option sous le nom de
distance_mur*)
val add_ennemi : int -> Point.t -> t -> t
