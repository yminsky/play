open Core.Std
open Geometry
open World

type world = { pos: posn;
               vel: posn;
             }

let square s color =
  let h = s /. 2. in
  let path = [ posn (-. h) (-. h)
             ; posn     h  (-. h)
             ; posn     h      h
             ; posn (-. h)     h
             ]
  in
  poly path color

let ship =
  rotate (square 25. black) ~deg:45.
  +< square 10. blue

let display w =
  empty_scene ~ll:(posn (-.200.) (-.200.)) ~ur:(posn 200. 200.) white
  ++ shift ship w.pos

let kick = 0.2

let () =
  big_bang
    { pos = origin; vel = origin }
    ~display
    ~tick:(fun w _ -> { w with pos = w.pos +! w.vel })
    ~key:(fun w k ->
      match k with
      | Up | (Char 'i') ->
        { w with vel = w.vel +! { origin with y =    kick }}
      | Down | Char 'k' ->
        { w with vel = w.vel +! { origin with y = -. kick }}
      | Left | Char 'j' ->
        { w with vel = w.vel +! { origin with x = -. kick }}
      | Right | Char 'l' ->
        { w with vel = w.vel +! { origin with x =    kick }}
      | Char 'h' ->
        { pos = origin; vel = origin }
      | Char 'q' ->
        exit 0
      | _ -> w
    )

