open Core.Std
open Geometry

type key =
| Char of Char.t
| Up
| Down
| Left
| Right
| Page_up
| Page_down


val big_bang
  :  display:('world -> scene)
  -> tick:('world -> Time.t -> 'world)
  -> key:('world -> key -> 'world)
  -> ?key_up:('world -> key -> 'world)
  -> 'world
  -> unit
