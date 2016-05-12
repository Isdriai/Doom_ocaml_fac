val display : Bsp.t -> Player.t -> unit

val taille : int

val angle_vision : int

val d_focale : int

val affiche_ennemi : Player.t -> int * Point.t -> unit

val go_solveur : Player.t -> (int * int ) list -> Bsp.t -> unit