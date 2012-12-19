open Core.Std
open Geometry
open World

type platform = { height: float;
		  start: float;
		  stop: float;
		}

let platform height start stop = { height; start; stop }

type world = { pos: posn;
               vel: posn;
	       plats: platform list
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

let ship =
  rotate (square 25. (color 0.3 0.3 1.)) ~deg:45.
  +< square 10. yellow

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
    ~f:(fun x p -> x +< platform_image p)

let display w =
  empty_scene ~ll:(posn (-.200.) (-.200.)) ~ur:(posn 200. 200.) white
  ++ platforms w.plats
  ++ shift ship w.pos

let rec on_some_plat plats posn =
  match plats with
  | [] -> false
  | hd :: tl ->
    if onplat hd posn then true
    else on_some_plat tl posn


let tick w _ =  
  if on_some_plat w.plats w.pos
  then
    if w.vel.y < 0.
    then { w with vel = origin;}
    else
      { w with  
	pos = w.pos +! w.vel;
	vel = w.vel +! posn 0. (-.0.008); 
      }
  else
    let new_vel =
      if w.vel.y < 3.
      then if w.vel.y > -3.
	then w.vel +! posn 0. (-.0.008)
	else { w.vel with y = -3.}  
      else { w.vel with y = 3.}
    in
    { w with
      pos = w.pos +! w.vel;
      vel = new_vel;
    }

let kick = 0.4

let world =
  { pos = origin; 
    vel = origin;
    plats = [ platform (-20.) 50. 100.     
	    ; platform 40. (-30.) 20.      
	    ; platform (-80.) (-30.) (150.)
	    ; platform 120.  (-30.) 30.    
	    ; platform (-195.) (-200.)200. 
	    ]
  }
      

let key w k =
  match k with
  | Up | (Char 'i') ->
    { w with vel = w.vel +! (posn 0. kick)}
  | Down | Char 'k' ->
    { w with vel = w.vel +! (posn 0. (-. kick))}
  | Left | Char 'j' ->
    { w with vel = w.vel +! (posn (-. kick) 0.)}
  | Right | Char 'l' ->
    { w with vel = w.vel +! (posn kick 0.)}
  | Char 'h' ->
    { w with pos = origin; vel = origin }
  | Char 'q' ->
    exit 0
  | _ -> w

let () =
  big_bang world
    ~display
    ~tick
    ~key

