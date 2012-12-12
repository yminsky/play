module Timer_ = Timer
open Core.Std
module Timer = Timer_

type posn = { x: float; y: float }
let posn x y = {x;y}
let (+!) a b = { x = a.x +. b.x;
                 y = a.y +. b.y;
               }
let (-!) a b = { x = a.x -. b.x;
                 y = a.y -. b.y;
               }

let rec timer_loop gap f =
  let start = Time.now () in
  f ();
  let stop = Time.now () in
  let until_goal = Time.diff (Time.add start gap) stop in
  Timer.set ~ms:(until_goal |! Time.Span.to_ms |! Float.to_int)
    ~callback:(fun () -> timer_loop gap f)

module type Shape = sig
  type t
  type spec

  val create : Widget.canvas Widget.widget -> spec -> t
  val spec : t -> spec
  val set_spec : t -> spec -> unit
end


module Make_shape (Spec : sig
  type t
  val to_xys : t -> (int * int) list
  val init : t -> Widget.canvas Widget.widget -> Tk.tagOrId
end) : Shape with type spec := Spec.t
  =
struct
  type t = { canvas : Widget.canvas Widget.widget;
             mutable spec: Spec.t;
             tag: Tk.tagOrId;
           }

  let create canvas spec =
    let tag = Spec.init spec canvas in
    { canvas; spec; tag }

  let spec t = t.spec

  let set_spec t spec =
    t.spec <- spec;
    Canvas.coords_set t.canvas t.tag ~xys:(Spec.to_xys spec)
end

module Circle_spec = struct
  type t = { pos: posn; radius: float }

  let to_xys {pos = {x;y}; radius} =
    let x1 = x -. radius and x2 = x +. radius in
    let y1 = y -. radius and y2 = y +. radius in
    let i = Float.to_int in
    [i x1,i y1; i x2,i y2]

  let init t canvas =
    let tag = Canvas.create_oval ~x1:0 ~y1:0 ~x2:0 ~y2:0 canvas
      ~fill:`Red ~outline:`Black
    in
    Canvas.coords_set canvas tag ~xys:(to_xys t);
    tag
end

module Circle = Make_shape(Circle_spec)

module Poly_spec = struct
  type t = posn list

  let to_xys t =
    List.map t ~f:(fun {x;y} ->
      let i = Int.of_float in
      (i x,i y))

  let init t canvas =
    Canvas.create_polygon
      ~xys:(to_xys t)
      ~fill:`Blue ~outline:`Black
      canvas
end

module Poly = Make_shape(Poly_spec)

module App = struct
  type t = { toplevel: Widget.toplevel Widget.widget;
             frame: Widget.frame Widget.widget;
             canvas: Widget.canvas Widget.widget;
           }

  let run ~width ~height =
    let toplevel = Tk.openTk ~clas:"Play" () in
    let frame = Frame.create ~width ~height toplevel in
    let canvas = Canvas.create ~width ~height frame in
    let t = { toplevel; frame; canvas } in
    let circle =
      let pos = {x = 20.; y = 20.} in
      let radius = 5. in
      Circle.create t.canvas {Circle_spec. pos; radius }
    in
    let square =
      Poly.create t.canvas [ posn 0.  0.
                           ; posn 20. 0.
                           ; posn 20. 20.
                           ; posn 0.  20. ]
    in
    Canvas.configure canvas ~background:`White;
    Pack.configure ~expand:true ~fill:`Both [frame];
    Pack.configure ~expand:true ~fill:`Both [canvas];
    timer_loop (sec 0.05) (fun () ->
      let spec = Circle.spec circle in
      Circle.set_spec circle
        { spec with Circle_spec.pos = spec.Circle_spec.pos +! {x=1.;y=0.} };
      Poly.set_spec square
        (Poly.spec square |! List.map ~f:((+!) {x=1.;y=0.5}));
    );
    Tk.mainLoop ()

end

let () =
  App.run ~width:1000 ~height:800


