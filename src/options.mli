type tmode = TwoD | ThreeD

val mode : tmode

val cin : in_channel

val win_w : int
val win_h : int

val ceiling_h : int
val floor_h : int
val wall_h : int

val fov : int

val step_dist : float

val xmin : float
val xmax : float

val scale : int
val minimap : bool
 
val debug : bool
val debug_bsp : bool

type tlang = Fr | Be
type touche = TZ | TQ | TS | TD | TA | TE | KKK | TC | TR | TF | TU | TB | TN | TV | TNone

val lang : tlang ref
val change_lang : tlang -> unit

val clavier_lg : char -> touche

val distance_mur : int

val nom_lab : string
val nom_solution : string

val max_affiche : int