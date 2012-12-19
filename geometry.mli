open Core.Std

type posn = { x: float; y: float }
val posn : float -> float -> posn
val (+!) : posn -> posn -> posn
val (-!) : posn -> posn -> posn
val negate : posn -> posn
val middle : posn -> posn -> posn
val origin : posn

type color = { r: float; g: float; b: float }

val color : float -> float -> float -> color
val black  : color
val blue   : color
val green  : color
val orange : color
val purple : color
val red    : color
val white  : color
val yellow : color

type base_image =
| Poly of posn * posn list * color

type image

val empty_image : image
val poly : posn list -> color -> image

val scale  : ?around:posn -> by:float  -> image -> image
val rotate : ?around:posn -> deg:float -> image -> image
val shift : image -> posn -> image
val recenter : image -> image

val (++) : image -> image -> image

(** Iterates over the [base_image]'s in the image, from lowest to highest,
    providing an offset for each [base_image].  *)
val image_iter : image -> f:(base_image -> unit) -> unit

type scene

val empty_scene : ll:posn -> ur:posn -> color -> scene
val scene_ll : scene -> posn
val scene_ur : scene -> posn
val image : scene -> image
val bg : scene -> color

val (+:) : scene -> image -> scene
