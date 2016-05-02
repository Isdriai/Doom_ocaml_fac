open Options
open Physic

type t = {
  mutable pos : Point.t;
  mutable pa : int;
}

let new_player pos pa = failwith "TODO"

type dir = Left | Right

let rotate d p = 
	match d with
	| Left -> expr
	| Right -> expr2

type mv = MFwd | MBwd | MLeft | MRight

let move d p bsp = failwith "TODO"
