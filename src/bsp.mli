type t = E | N of Segment.t * t * t | Ennemi of int

val parse : (Segment.t -> 'a) -> t -> Point.t -> unit

val rev_parse : (Segment.t -> 'a) -> t -> Point.t -> unit

val iter : (Segment.t -> 'a) -> t -> unit

val build_bsp : Segment.t list -> t

val remove_ennemi : int -> Point.t -> t -> t

val add_ennemi : int -> Point.t -> t -> t