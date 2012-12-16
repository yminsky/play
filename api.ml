open Core.Std

module type API = sig

  type posn = { x: float; y: float }

  val posn   : float -> float -> posn
  val origin : posn
  val (+!)   : posn -> posn -> posn
  val (-!)   : posn -> posn -> posn
  val scale  : posn -> float -> posn

  type color = { r: int; g: int; b: int }

  val color : int -> int -> int -> color

  val black  : color
  val blue   : color
  val green  : color
  val orange : color
  val purple : color
  val red    : color
  val white  : color
  val yellow : color

  type scene

  val scene : float -> float -> color -> scene

  type image

  val poly          : posn list -> color -> image
  val line          : posn -> posn -> color -> image
  val circle        : posn -> float -> color -> image
  val overlay       : image list -> image
  val shift         : image -> posn -> image
  val set_pin       : image -> posn -> image
  val pin_to_center : image -> image
  val pin           : image -> posn option

  val (+:) : scene -> image -> scene

  val rotate_posn
    :  float
    -> posn
    -> posn

  val rotate
    : float
    -> image
    -> image

  val big_bang
    :  'world
    -> draw    : ('world -> scene)
    -> tick    : ('world -> 'world)
    -> unit
end

module M(API:API) = struct
  open API

  let mag p =
    sqrt (p.x *. p.x +. p.y *. p.y)

  let square size =
    let h = size /. 2. in
    poly [ posn (-. h) (-. h) ;
           posn     h  (-. h) ;
           posn     h      h  ;
           posn (-. h)     h  ;
         ]
      blue

  let boxes =
    scene 500. 500. white
    +: square 5.
    +: shift (square 5.) (posn 100. 70.)

  type world = {
    pos: posn;
    vel: posn;
  }

  let grav = 0.01

  let tick w =
    { pos = w.pos +! w.vel;
      vel = { w.vel with y = w.vel.y -. grav };
    }

  let draw w =
    scene 500. 500. white
    +: shift (square 5.) w.pos

  let () =
    big_bang
      { pos = posn 0. 0.;
        vel = posn 12. 6.; }
      ~draw
      ~tick

end
