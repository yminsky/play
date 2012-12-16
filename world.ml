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
  | Circle _ -> assert false
  | Line _ -> assert false
  | Poly (first,rest,color) ->
    GlDraw.begins `polygon;
    GlDraw.color (gl_color color);
    List.iter (first::rest) ~f:(fun posn ->
      GlDraw.vertex2 (posn.x,posn.y));
    GlDraw.ends ()

let big_bang world ~display ~tick ~key =
  let command =
    (* Setup some shared state *)
    let world = ref (world,display world) in
    let render () =
      let scene = snd !world in
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
    let set_world new_world =
      world := (new_world, display new_world)
    in
    let rec timer_loop () =
      let start = Time.now () in
      let world' = tick (fst !world) start in
      let new_world = tick world' start in
      set_world new_world;
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
        +> flag "-width"  (optional_with_default 512 int) ~doc:" Screen width"
        +> flag "-height" (optional_with_default 512 int) ~doc:" Screen height"
      )
      (fun w h () ->
        Glut.initDisplayMode ~double_buffer:true ();
        Glut.initWindowSize ~w ~h;
        ignore (Glut.createWindow ~title:"Play");
        GlClear.color (gl_color (bg (snd !world)));
        GlDraw.shade_model `smooth;
        timer_loop ();
        Glut.displayFunc ~cb:render;
        Glut.idleFunc ~cb:(Some Glut.postRedisplay);
        Glut.keyboardFunc ~cb:(fun ~key:key_num ~x:_ ~y:_ ->
          set_world (key (fst !world) (Char (Char.of_int_exn key_num)))
        );
        Glut.keyboardFunc ~cb:(fun ~key:key_num ~x:_ ~y:_ ->
          set_world (key (fst !world) (Char (Char.of_int_exn key_num)))
        );
        Glut.specialFunc ~cb:(fun ~key:special_key ~x:_ ~y:_ ->
          let new_key = special_key_to_key special_key in
          Option.iter new_key ~f:(fun k ->
            set_world (key (fst !world) k)));
        Glut.mainLoop ()
      )
  in
  let argv = Array.to_list (Glut.init ~argv:Sys.argv) in
  Command.run ~argv command

