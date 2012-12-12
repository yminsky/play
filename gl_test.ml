open Core.Std


let init () =
  let _argv = Glut.init ~argv:Sys.argv in
  Glut.initDisplayMode ();
  Glut.initWindowPosition ~x:0 ~y:0;
  Glut.initWindowSize ~w:500 ~h:500;
  let _win_id = Glut.createWindow ~title:"foo" in
  Glut.mainLoop ()
;;


let () = init ()
