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
  :  'world
  -> display:('world -> scene)
  -> tick:('world -> Time.t -> 'world)
  -> key:('world -> key -> 'world)
  -> unit
