let pi = 4. *. atan 1.

let time =
  let start = Unix.gettimeofday () in
  fun () -> Unix.gettimeofday () -. start

let strips = 32

let render_annulus =
  let annulus_list = ref None in
  fun () -> match !annulus_list with
  | Some l -> GlList.call l
  | None ->
    annulus_list := Some (GlList.create `compile_and_execute);
    GlDraw.begins `triangle_strip;
    for i = 0 to strips do
      let theta = 2. *. pi *. float i /. float strips in
      let x = sin theta and y = cos theta in
      List.iter GlDraw.vertex2 [1.5 *. x, 1.5 *. y; 2. *. x,
                                2. *. y]
    done;
    GlDraw.ends ();
    GlList.ends ()

let render () =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.perspective ~fovy:45.0 ~aspect:1. ~z:(0.1, 1000.);
  GluMat.look_at ~eye:(3., 3., -5.) ~center:(0., 0., 0.) ~up:(0., 1., 0.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.color (0.6, 1., 0.8);
  GlClear.clear [`color; `depth];
  Gl.enable `depth_test;
  GlDraw.color (0.5 *. (1. +. sin(time ())), 0., 1.);
  GlMat.scale ~x:0.1 ~y:0.1 ~z:0.1 ();
  for i = 1 to 20 do
    GlMat.rotate ~angle:(-10. *. time ()) ~x:0.25
      ~y:1. ();
    GlMat.translate ~x:(-2.5) ()
  done;
  for i = 1 to 40 do
    GlMat.translate ~x:2.5 ();
    GlMat.rotate ~angle:(10. *. time ()) ~x:0.25
      ~y:1. ();
    render_annulus ()
  done;
  Gl.flush ();
  Glut.swapBuffers ()

let _ =
  let _ = Glut.init Sys.argv in
  Glut.initDisplayMode ~depth:true ~double_buffer:true ();
  Glut.initWindowSize ~w:512 ~h:512;
  ignore(Glut.createWindow ~title:"Annuli OpenGL demo");
  Glut.displayFunc ~cb:render;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key=27 then exit 0);
  Glut.mainLoop ()
