open Core.Std
open Geometry

let start = Time.now ()

let square s color =
  let h = s /. 2. in
  let path = [ posn (-. h) (-. h)
             ; posn     h  (-. h)
             ; posn     h      h
             ; posn (-. h)     h
             ]
  in
  poly path color

let build_scene now =
  let sec = Time.Span.to_sec (Time.diff now start) in
  let scale im = scale  im ~by:(sec *. 2.)   in
  let rot im   = rotate im ~deg:(sec *. 60.) in
  empty_scene ~ll:(posn (-.100.) (-.100.)) ~ur:(posn 100. 100.) yellow
  ++ rot (shift (scale (square 20. orange)) (posn 10. 40.))
  ++ shift (square 40. red)  (posn (-10.) (-10.))
  ++ rot (square 10. green)

let gl_color {r;g;b} = (r,g,b)

let command =
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
      let scene = build_scene (Time.now ()) in
      GlClear.color (gl_color (bg scene));
      GlDraw.shade_model `smooth;
      let render () =
        let scene = build_scene (Time.now ()) in
        GlMat.mode `projection;
        GlMat.load_identity ();
        let ll = scene_ll scene in
        let ur = scene_ur scene in
        GluMat.ortho2d ~x:(ll.x,ur.x) ~y:(ll.y, ur.y);
        GlMat.mode `modelview;
        GlMat.load_identity ();
        GlClear.clear [ `color ];
        image_iter (image scene) ~f:(fun base ->
          match base with
          | Circle _ -> assert false
          | Line _ -> assert false
          | Poly (first,rest,color) ->
            GlDraw.begins `polygon;
            GlDraw.color (gl_color color);
            List.iter (first::rest) ~f:(fun posn ->
              GlDraw.vertex2 (posn.x,posn.y));
            GlDraw.ends ();
        );
        Gl.flush ();
        Glut.swapBuffers ()
      in
      Glut.displayFunc ~cb:render;
      Glut.idleFunc ~cb:(Some Glut.postRedisplay);
      Glut.keyboardFunc ~cb:(fun ~key ~x:_ ~y:_ ->
        printf "%4d -- %c\n%!" key (Char.of_int_exn key);
        if Char.of_int key = Some 'q' then exit 0
      );
      Glut.mainLoop ()
    )

let () =
  let argv = Array.to_list (Glut.init ~argv:Sys.argv) in
  Command.run ~argv command

