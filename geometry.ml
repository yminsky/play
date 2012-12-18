open Core.Std

type posn = { x: float; y: float }

let posn x y = {x;y}

let (+!) a b = { x = a.x +. b.x;
                 y = a.y +. b.y;
               }
let (-!) a b = { x = a.x -. b.x;
                 y = a.y -. b.y;
               }

let smult p c = { x = p.x *. c
                ; y = p.y *. c
                }

let pi = 2. *. asin 1.

let d2r deg =
  pi *. deg /. 180.

(* cosine and sine *)
type cs = { cos: float; sin: float }

let fast_rot {x;y} {cos;sin} =
  { x =    cos *. x +. y *. sin
  ; y = -. sin *. x +. y *. cos
  }

let cs_of_deg deg =
  let rad = d2r deg in
  let cos = cos rad in
  let sin = sin rad in
  { cos; sin }

let rot p deg =
  fast_rot p (cs_of_deg deg)

let _ = rot

let scale_posn_around p ~around ~by =
  smult (p -! around) by
  +! around

let middle p1 p2 =
  smult (p1 +! p2) 0.5

let origin = { x = 0.; y = 0. }
let negate p = { x = -. p.x; y = -. p.y }
;;

type color = { r: float; g: float; b: float }
let color r g b = {r;g;b}


let black  = color 0.   0.   0.
let white  = color 1.   1.   1.
let blue   = color 0.   0.   1.
let green  = color 0.   1.   0.
let orange = color 1.   0.5  0.
let purple = color 1.   0.   1.
let red    = color 1.   0.   0.
let yellow = color 1.   1.   0.

type base_image =
| Poly   of posn * posn list * color

let merge_posn p1 p2 ~f =
  { x = f p1.x p2.x
  ; y = f p1.y p2.y
  }

let posn_min = merge_posn ~f:Float.min
let posn_max = merge_posn ~f:Float.max

let base_corners = function
  | Poly (p, ps, _) ->
    let corner merge = List.fold ~f:merge ~init:p ps in
    (corner posn_min, corner posn_max)

let merge_corners (ll1,ur1) (ll2,ur2) =
  (posn_min ll1 ll2, posn_max ur1 ur2)

let scale_base_image_around b ~around ~by =
  let sp = scale_posn_around ~around ~by in
  match b with
  | Poly (p, ps,c) ->
    Poly (sp p, List.map ~f:sp ps, c)

type combo =
| Base of base_image
| Top_bottom of combo * combo

let rec combo_corners = function
  | Base b -> base_corners b
  | Top_bottom (t,b) ->
    merge_corners (combo_corners t) (combo_corners b)

let rec combo_map combo ~f =
  match combo with
  | Top_bottom (t,b) ->
    let c = combo_map ~f in
    Top_bottom (c t, c b)
  | Base b -> Base (f b)

type image = combo option

let corners image =
  Option.map image ~f:combo_corners

let rotate ?(around=origin) ~deg image  =
  Option.map image ~f:(fun combo ->
    let cs = cs_of_deg deg in
    let rot p = fast_rot (p -! around) cs +! around in
    combo_map combo ~f:(function
    | Poly (p,ps,c) -> Poly (rot p, List.map ~f:rot ps, c)
    ))

let scale_around image ~around ~by =
  Option.map image ~f:(fun combo ->
    combo_map ~f:(scale_base_image_around ~around ~by) combo)

let empty_image = None

let scale ?(around=origin) ~by im =
  scale_around im ~around ~by

let poly edges color =
  match edges with
  | [] -> failwith "Polygon should have at least one point"
  | hd :: tl ->
    Some (Base (Poly (hd,tl,color)))

let shift image offset =
  Option.map image ~f:(fun combo ->
    if offset.x = 0. && offset.y = 0. then combo
    else
      let combo = combo_map combo ~f:(function
        | Poly (p,ps,c) ->
          let f p = p +! offset in
          Poly (f p, List.map ~f ps, c)
      )
      in
      combo
  )

let recenter image =
  match corners image with
  | None -> image
  | Some (ll,ur) ->
    let mid = smult (ll +! ur) 0.5 in
    shift image (negate mid)

let overlay ~topfirst i1 i2 =
  match (i1,i2) with
  | None, (_ as x) | (_ as x), None -> x
  | Some c1, Some c2 ->
    Some (
      if topfirst
      then Top_bottom (c1,c2)
      else Top_bottom (c2,c1)
    )

let (++) = overlay ~topfirst:false

let image_iter image ~f =
  Option.iter image ~f:(fun combo ->
    let rec iter combo =
      match combo with
      | Base base_image -> f base_image
      | Top_bottom (t,b) -> iter b; iter t
    in
    iter combo
  )

type scene = { bg: color
             ; ll: posn
             ; ur: posn
             ; image : image
             }

let empty_scene ~ll ~ur bg =
  { bg; ll; ur ; image = empty_image }

let scene_ll s = s.ll
let scene_ur s = s.ur
let image s = s.image
let bg s = s.bg

let (+:) s i =
  { s with image = s.image ++ i }


