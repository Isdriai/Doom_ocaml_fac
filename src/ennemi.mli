
type t = {ide : int; mutable position : Point.t;}

val new_ennemi : Point.t -> t
val add : t -> Bsp.t ->Bsp.t
