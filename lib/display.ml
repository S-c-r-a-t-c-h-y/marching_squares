open Algorithm
open Images

let hande_graph_closing () : unit =
  while Graphics.read_key () <> 'q' do
    ()
  done;
  Graphics.close_graph ()

let display_rgb_image (img : rgb_image) ?(scale : int = 1)
    ?(open_window : bool = true) (x : int) (y : int) =
  if open_window then
    Printf.sprintf " %dx%d" (img.width * scale) (img.height * scale)
    |> Graphics.open_graph;
  Graphics.draw_image
    (Graphics.make_image
       (Array.map
          (Array.map (fun (r, g, b) -> Graphics.rgb r g b))
          (Algorithm.scale_matrix img.pixels img.height img.width scale)))
    x y;
  if open_window then hande_graph_closing ()

let draw_contour (contour : contour_line) (scale : int) =
  let scale = float_of_int scale in
  match contour with
  | Empty | Full -> ()
  | Line (point1, point2) ->
      Graphics.moveto
        (point1.x *. scale |> int_of_float)
        (point1.y *. scale |> int_of_float);
      Graphics.lineto
        (point2.x *. scale |> int_of_float)
        (point2.y *. scale |> int_of_float)

let rec draw_contours (contours : contour_line list) ~(scale : int) =
  match contours with
  | [] -> ()
  | e :: q ->
      draw_contour e scale;
      draw_contours q ~scale

let march_and_draw (g : int array array) (n : int) (m : int) (delta : int)
    (stepX : int) (stepY : int) (scale : int) : unit =
  Printf.sprintf " %dx%d" ((m - 1) * scale) ((n - 1) * scale)
  |> Graphics.open_graph;
  Graphics.set_window_title "Marching squares OCaml";
  marching_squares g n m delta stepX stepY |> draw_contours ~scale;
  hande_graph_closing ()

let march_and_draw_ppm (filename : string) (delta : int) (stepX : int)
    (stepY : int) (scale : int) : unit =
  let ppm = open_ppm filename in
  let rgb = Images.ppm_to_rgb ppm in
  let n = ppm.height and m = ppm.width in
  Printf.sprintf " %dx%d" (m * scale) (n * scale) |> Graphics.open_graph;
  Graphics.set_color Graphics.red;
  Graphics.set_line_width 2;
  Graphics.set_window_title "Marching squares OCaml";
  display_rgb_image rgb ~scale ~open_window:false 0 0;
  marching_squares
    (match ppm.pixels with
    | P1 pixels | P2 pixels -> pixels
    | P3 pixels -> Images.to_gray_scale pixels)
    n m delta stepX stepY
  |> draw_contours ~scale;
  hande_graph_closing ()
