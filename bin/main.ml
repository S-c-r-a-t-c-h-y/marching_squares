(*
   INPUT : G : matrice m * n de scalaires
           stepX, stepY la résolution d'échantilonage
           delta une isovaleur

   OUTPUT : un set gamma de lignes de contours
*)

open Graphics
(* open Images *)

type coordonnee = { x : float; y : float }
type contour_line = Empty | Full | Line of coordonnee * coordonnee

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

let random_array n ?(minimum = 0) maximum =
  let array = Array.make n 0 in
  for i = 0 to n - 1 do
    array.(i) <- minimum + Random.int (maximum - minimum)
  done;
  array

let random_matrix n m ?(minimum = 0) maximum =
  Array.init n (fun _ -> random_array m ~minimum maximum)

let lut (kappa : int) (north : coordonnee) (east : coordonnee)
    (south : coordonnee) (west : coordonnee) : contour_line list =
  match kappa with
  | 0 -> [ Empty ]
  | 1 -> [ Line (south, west) ]
  | 2 -> [ Line (south, east) ]
  | 3 -> [ Line (east, west) ]
  | 4 -> [ Line (north, east) ]
  | 5 -> [ Line (north, east); Line (west, south) ]
  | 6 -> [ Line (north, south) ]
  | 7 -> [ Line (north, west) ]
  | 8 -> [ Line (north, west) ]
  | 9 -> [ Line (north, south) ]
  | 10 -> [ Line (north, west); Line (east, south) ]
  | 11 -> [ Line (north, east) ]
  | 12 -> [ Line (east, west) ]
  | 13 -> [ Line (south, east) ]
  | 14 -> [ Line (south, west) ]
  | 15 -> [ Full ]
  | _ -> failwith "invalid isocontour"

(* n lignes, m colonnes *)
let sample_grid (g : int array array) (n : int) (m : int) (delta : int)
    (stepX : int) (stepY : int) :
    int array array * coordonnee array array * int * int =
  let p = n / stepY and q = m / stepX in
  let f = Array.make_matrix p q 0
  and coord =
    Array.init p (fun _ -> Array.init q (fun _ -> { x = 0.; y = 0. }))
  in
  for i = 0 to p - 1 do
    for j = 0 to q - 1 do
      if g.(i * stepY).(j * stepX) > delta then f.(i).(j) <- 0
      else f.(i).(j) <- 1;
      coord.(i).(j) <-
        { x = j * stepX |> float_of_int; y = i * stepY |> float_of_int }
    done
  done;
  (f, coord, p, q)

let march (f : int array array) (coord : coordonnee array array) (p : int)
    (q : int) : contour_line list =
  let gamma = ref [] in
  for i = 0 to p - 2 do
    for j = 0 to q - 2 do
      let a = coord.(i).(j)
      and b = coord.(i).(j + 1)
      and c = coord.(i + 1).(j + 1)
      and d = coord.(i + 1).(j)
      and kappa =
        (8 * f.(i).(j))
        + (4 * f.(i).(j + 1))
        + (2 * f.(i + 1).(j + 1))
        + f.(i + 1).(j)
      in
      let north = { x = (a.x +. b.x) /. 2.; y = a.y }
      and east = { x = b.x; y = (b.y +. c.y) /. 2. }
      and south = { x = (d.x +. c.x) /. 2.; y = d.y }
      and west = { x = a.x; y = (a.y +. d.y) /. 2. } in
      Printf.printf
        "north: %f %f, east: %f %f, south: %f %f, west: %f %f, kappa:%d\n"
        north.x north.y east.x east.y south.x south.y west.x west.y kappa;
      gamma := !gamma @ lut kappa north east south west
    done
  done;
  !gamma

(* n lignes, m colonnes *)
let marching_squares (g : int array array) (n : int) (m : int) (delta : int)
    (stepX : int) (stepY : int) : contour_line list =
  let f, coord, p, q = sample_grid g n m delta stepX stepY in
  march f coord p q

let hande_graph_closing () : unit =
  while Graphics.read_key () <> 'q' do
    ()
  done;
  Graphics.close_graph ()

let march_and_draw (g : int array array) (n : int) (m : int) (delta : int)
    (stepX : int) (stepY : int) (scale : int) : unit =
  Printf.sprintf " %dx%d" ((m - 1) * scale) ((n - 1) * scale)
  |> Graphics.open_graph;
  Graphics.set_window_title "Marching squares Ocaml";
  marching_squares g n m delta stepX stepY |> draw_contours ~scale;
  hande_graph_closing ()

let test_cloud =
  [|
    [| 255; 255; 255; 255; 255 |];
    [| 255; 255; 182; 255; 255 |];
    [| 255; 182; 182; 182; 255 |];
    [| 255; 182; 182; 182; 255 |];
    [| 255; 255; 255; 255; 255 |];
  |]
