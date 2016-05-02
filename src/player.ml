open Options
open Physic

type t = {
  mutable pos : Point.t;
  mutable pa : int;
}

let new_player pos pa = failwith "TODO"

type dir = Left | Right

let rotate d p = failwith "TODO"

type mv = MFwd | MBwd | MLeft | MRight

let move d p bsp = failwith "TODO"
