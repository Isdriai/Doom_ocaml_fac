type tmode = TwoD | ThreeD

let usage = "usage: ./bsp file.lab"
let file = ref ""

let mode = ref TwoD

let win_w = ref 800
let win_h = ref 800

let fov = ref 60

let step_dist = ref 10

let distance_mur = 10

let xmin = ref 1
let xmax = 9000.

let scale = ref 5
let minimap = ref false

let debug = ref false
let debug_bsp = ref false

let set_mode = function
  | "2D" -> mode := TwoD
  | "3D" -> mode := ThreeD
  | _ -> raise (Arg.Bad "2D or 3D only")


let specs = 
  [ "-mode", Arg.String set_mode, "<2D | 3D> 2D or 3D display";
    "-fov", Arg.Set_int fov, " field of vision (angle de vision)";
    "-dims", Arg.Tuple [Arg.Set_int win_w; Arg.Set_int win_h], 
    " set the dimensions of the graph";
    "-scale", Arg.Set_int scale, " scale of the 2D map";
    "-map", Arg.Set minimap, " set a minimap in the lower left corner";
    "-step", Arg.Set_int step_dist, " set the distance between two steps";
    "-xmin", Arg.Set_int xmin, " set minimum distance of display";
    "-debug", Arg.Set debug, " debugging 2D rendering";
    "-debugbsp", Arg.Set debug_bsp, " debugging bsp";
  ]

let alspecs = Arg.align specs

let cin =
  let ofile = ref None in
  let set_file s =
    if Filename.check_suffix s ".lab" then ofile := Some s
    else raise (Arg.Bad "no .lab extension");
  in
  Arg.parse alspecs set_file usage;
  match !ofile with 
    | Some f -> file := f ; open_in f
    | None -> raise (Arg.Bad "no file provided")


let file = !file

let win_w = !win_w
let win_h = !win_h

let xmin = float !xmin

let ceiling_h = win_h / 4
let floor_h = 0
let wall_h = ceiling_h - floor_h

let mode = !mode

let fov = !fov

let step_dist = float !step_dist

let scale = !scale
let minimap = !minimap

let debug = !debug
let debug_bsp = !debug_bsp

type tlang = Fr | Be
let lang = ref Fr
let change_lang langue = lang := langue

type touche = TZ | TQ | TS | TD | TA | TE | KKK | TC | TR | TF | TU | TB | TN | TV | TNone

let clavier_lg touche = match !lang with
  |Fr -> (match touche with 
          |'z' -> TZ
          |'q' -> TQ
          |'s' -> TS
          |'d' -> TD
          |'a' -> TA
          |'e' -> TE
          |'k' -> KKK
          |'c' -> TC
          |'r' -> TR
          |'f' -> TF
          |'u' -> TU
          |'b' -> TB
          |'n' -> TN
          |'v' -> TV
          |_ -> TNone)
  |Be -> (match touche with 
          |'\233' -> TZ
          |'a' -> TQ
          |'u' -> TS
          |'i' -> TD
          |'b' -> TA
          |'p' -> TE
          |'s' -> KKK
          |'x' -> TC
          |'o' -> TR
          |'e' -> TF
          |'v' -> TU
          |_ -> TNone)

let nom_lab = "labyrinthes/random_lab.lab"

let nom_solution = "labyrinthes/solution"

let max_affiche = 5000