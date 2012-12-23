open Core.Std
open Geometry
open World

type thrust =
  { up: bool;
    down: bool;
    left: bool;
    right: bool;
  }

let init_thrust =
  { up    = false;
    down  = false;
    left  = false;
    right = false;
  }

let kick = 0.06
let grav = posn 0. (-0.015)

let acc_of_thrust thrust =
  let zero = posn 0. 0. in
  (if thrust.up then posn 0. (-. kick) else zero)
  +! (if thrust.down then posn 0. kick else zero)
  +! (if thrust.left then posn kick 0. else zero)
  +! (if thrust.right then posn (-.kick) 0. else zero)

let some_thrust {left;right;up;down} =
  up || left || right || down

type dir = Dir_up | Dir_down | Dir_left | Dir_right

let get_thrust thrust dir =
  match dir with
  | Dir_up -> thrust.up
  | Dir_down -> thrust.down
  | Dir_left -> thrust.left
  | Dir_right -> thrust.right

type platform = { height: float;
                  start: float;
                  stop: float;
                }

let platform height start stop = { height; start; stop }

type world = { pos: posn;
               vel: posn;
               plats: platform list;
               thrust: thrust;
               time: Time.t;
             }

let onplat plat posn =
  ((posn.x <  plat.stop) && (posn.x > plat.start))
  && (posn.y > (plat.height +. 5.) && (posn.y < (plat.height +. 10.)))

let square s color =
  let h = s /. 2. in
  let path = [ posn (-. h) (-. h)
             ; posn     h  (-. h)
             ; posn     h      h
             ; posn (-. h)     h
             ]
  in
  poly path color

let platform_image p =
  let path = [ posn p.start p.height
             ; posn p.stop  p.height
             ; posn p.stop  (p.height -. 5.)
             ; posn p.start (p.height -. 5.)
             ]
  in
  poly path orange

let platforms ps =
  List.fold ps ~init:empty_image
    ~f:(fun x p -> x ++ platform_image p)

let bg =
  empty_scene ~ll:(posn (-.500.) (-.400.)) ~ur:(posn 500. 400.) white

let sqrt3 = sqrt 3.

let tri w c =
  poly [ posn (-. w /. 2.) 0.
       ; posn (w /. 2.) 0.
       ; posn 0. (w *. sqrt3 /. 2.)
       ]
    c

let flicker time c1 c2 =
  let cond =
    Time.to_ofday time (Zone.machine_zone ())
    |! Time.Ofday.to_span_since_start_of_day
    |! Time.Span.to_us
    |! (fun x -> Float.to_int x % 2 = 0)
  in
  if cond then c1 else c2

let ship thrust time =
  let base_color = color 0.3 0.3 1. in
  let corner dir =
    if not (get_thrust thrust dir) then empty_image
    else
      let base =
        let t = (tri 20. (flicker time red orange)) in
        shift t (posn 0. 10.)
      in
      match dir with
      | Dir_up -> base
      | Dir_down -> rotate base ~deg:180.
      | Dir_left -> rotate base ~deg:270.
      | Dir_right -> rotate base ~deg:90.
 in
  rotate (square 25. base_color) ~deg:45.
  ++ square 10. yellow
  ++ corner Dir_up
  ++ corner Dir_down
  ++ corner Dir_left
  ++ corner Dir_right

let display w =
  bg
  +: platforms w.plats
  +: shift (ship w.thrust w.time) w.pos

let rec on_some_plat plats posn =
  match plats with
  | [] -> false
  | hd :: tl ->
    if onplat hd posn then true
    else on_some_plat tl posn

let tick w time =
  if on_some_plat w.plats w.pos && w.vel.y < 0.
  then
    { w with vel = origin; time}
  else
    let new_vel =
      if w.vel.y > 5.       then { w.vel with y =  5.}
      else if w.vel.y < -5. then { w.vel with y = -5.}
      else w.vel +! grav +! acc_of_thrust w.thrust
    in
    let wrap pos =
      if pos.x <  (scene_ll bg).x then
        { pos with x = (scene_ur bg).x }
      else if pos.x > (scene_ur bg).x then
        { pos with x = (scene_ll bg).x }
      else
        pos
    in
    { w with
      pos = wrap (w.pos +! w.vel);
      vel = new_vel;
      time;
    }


let world =
  { pos = origin;
    vel = origin;
    plats = [ platform (-20.) 50. 100.
            ; platform 40. (-30.) 20.
            ; platform (-80.) (-30.) (150.)
            ; platform 120.  (-30.) 30.
            ; platform (-195.) (-500.) 500.
            ];
    thrust = init_thrust;
    time = Time.now ();
  }


let key w k =
  match k with
  | Up | (Char 'i') ->
    { w with thrust = { w.thrust with up = true }}
  | Down | Char 'k' ->
    { w with thrust = { w.thrust with down = true }}
  | Left | Char 'j' ->
    { w with thrust = { w.thrust with left = true }}
  | Right | Char 'l' ->
    { w with thrust = { w.thrust with right = true }}
  | Char 'h' ->
    { w with pos = origin; vel = origin }
  | Char 'q' ->
    exit 0
  | _ -> w

let key_up w k =
  match k with
  | Up | (Char 'i') ->
    { w with thrust = { w.thrust with up = false }}
  | Down | Char 'k' ->
    { w with thrust = { w.thrust with down = false }}
  | Left | Char 'j' ->
    { w with thrust = { w.thrust with left = false }}
  | Right | Char 'l' ->
    { w with thrust = { w.thrust with right = false }}
  | _ -> w

let () =
  big_bang world
    ~display
    ~tick
    ~key
    ~key_up

(*
let scene = empty_scene ~ll:(posn 0. 0.) ~ur:(posn 1. 1.) blue
let () =
  big_bang ()
    ~display:(fun () -> scene)
    ~tick:(fun () _ -> ())
    ~key:(fun () _ -> ())
*)
