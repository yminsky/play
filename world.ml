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

let special_key_to_key = function
  | Glut.KEY_UP -> Some Up
  | Glut.KEY_DOWN -> Some Down
  | Glut.KEY_PAGE_UP -> Some Page_up
  | Glut.KEY_PAGE_DOWN -> Some Page_down
  | Glut.KEY_LEFT -> Some Left
  | Glut.KEY_RIGHT -> Some Right
  | _ -> None

let gl_color {r;g;b} = (r,g,b)

let draw_base_image = function
  | Poly (first,rest,color) ->
    GlDraw.begins `polygon;
    GlDraw.color (gl_color color);
    List.iter (first::rest) ~f:(fun posn ->
      GlDraw.vertex2 (posn.x,posn.y));
    GlDraw.ends ()

module State : sig
  type 'a t
  val create : 'a -> ('a -> scene) -> 'a t
  val set : 'a t -> 'a -> unit
  val world : 'a t -> 'a
  val scene : 'a t -> scene
end = struct
  type 'a t = { to_scene: 'a -> scene;
                mutable world_and_scene: 'a * scene Lazy.t;
              }

  let create w to_scene =
    { to_scene;
      world_and_scene = (w, lazy (to_scene w));
    }

  let set t w =
    t.world_and_scene <- (w, lazy (t.to_scene w))

  let world t = fst t.world_and_scene
  let scene t = Lazy.force (snd t.world_and_scene)
end

let aspect_ratio scene =
  let ll = scene_ll scene in
  let ur = scene_ur scene in
  let diff = ur -! ll in


let big_bang world ~display ~tick ~key =
  let command =
    (* Setup some shared state *)
    let state = State.create world display in
    let render () =
      let scene = State.scene state in
      GlMat.mode `projection;
      GlMat.load_identity ();
      let ll = scene_ll scene in
      let ur = scene_ur scene in
      GluMat.ortho2d ~x:(ll.x,ur.x) ~y:(ll.y, ur.y);
      GlMat.mode `modelview;
      GlMat.load_identity ();
      GlClear.clear [ `color ];
      image_iter (image scene) ~f:draw_base_image;
      Gl.flush ();
      Glut.swapBuffers ()
    in
    let granularity = sec 0.02 in
    let rec timer_loop () =
      let start = Time.now () in
      let world = tick (State.world state) start in
      State.set state world;
      let after = Time.now () in
      let next = Time.add start granularity in
      let remaining = Time.diff next after in
      Glut.timerFunc ~ms:(Float.to_int (Time.Span.to_ms remaining))
        ~value:() ~cb:(fun ~value:() -> timer_loop ())
    in
    Command.basic
      ~summary:"Test OpenGL program"
      Command.Spec.(
        empty
        +> flag "-width"  (optional_with_default 512 int) ~doc:" Max screen width"
        +> flag "-height" (optional_with_default 512 int) ~doc:" Max screen height"
      )
      (fun w h () ->
        Glut.initDisplayMode ~double_buffer:true ();
        Glut.initWindowSize ~w ~h;
        ignore (Glut.createWindow ~title:"Play");
        GlClear.color (gl_color (bg (State.scene state)));
        GlDraw.shade_model `smooth;
        timer_loop ();
        Glut.displayFunc ~cb:render;
        Glut.idleFunc ~cb:(Some Glut.postRedisplay);
        Glut.keyboardFunc ~cb:(fun ~key:key_num ~x:_ ~y:_ ->
          State.set state (key (State.world state)
                             (Char (Char.of_int_exn key_num)))
        );
        Glut.keyboardFunc ~cb:(fun ~key:key_num ~x:_ ~y:_ ->
          State.set state (key (State.world state)
                             (Char (Char.of_int_exn key_num)))
        );
        Glut.specialFunc ~cb:(fun ~key:special_key ~x:_ ~y:_ ->
          let new_key = special_key_to_key special_key in
          Option.iter new_key ~f:(fun k ->
            State.set state (key (State.world state) k)));
        Glut.mainLoop ()
      )
  in
  let argv = Array.to_list (Glut.init ~argv:Sys.argv) in
  Command.run ~argv command

