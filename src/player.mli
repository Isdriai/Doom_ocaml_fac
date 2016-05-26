type t = {
  mutable pos : Point.t;
  mutable pa : int;
  mutable courir : bool;
  pos_i : Point.t;
  pa_i : int;
  mutable color : Graphics.color;
}

val new_player : ?g:Graphics.color -> Point.t -> int -> t

type dir = Left | Right

val rotate : ?angle:int -> dir -> t -> unit

type mv = MFwd | MBwd | MLeft | MRight

val move : mv -> t -> Bsp.t -> unit

val courir : t -> unit

val reset : t -> unit

val tire : t -> Ennemi.t list ref -> Bsp.t -> Bsp.t