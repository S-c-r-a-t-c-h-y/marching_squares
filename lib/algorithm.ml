type coordonnee = { x : float; y : float }
type contour_line = Empty | Full | Line of coordonnee * coordonnee

let random_array n ?(minimum = 0) maximum =
  let array = Array.make n 0 in
  for i = 0 to n - 1 do
    array.(i) <- minimum + Random.int (maximum - minimum)
  done;
  array

let random_matrix n m ?(minimum = 0) maximum =
  Array.init n (fun _ -> random_array m ~minimum maximum)

let scale_matrix matrix n m scale =
  let scale_line arr m scale =
    Array.init (m * scale) (fun i -> arr.(i / scale))
  in
  Array.init (n * scale) (fun i -> scale_line matrix.(i / scale) m scale)

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
      (* Printf.printf
         "north: %f %f, east: %f %f, south: %f %f, west: %f %f, kappa:%d\n"
         north.x north.y east.x east.y south.x south.y west.x west.y kappa; *)
      gamma := !gamma @ lut kappa north east south west
    done
  done;
  !gamma

(* n lignes, m colonnes *)
let marching_squares (g : int array array) (n : int) (m : int) (delta : int)
    (stepX : int) (stepY : int) : contour_line list =
  let reverse array =
    let len = Array.length array in
    for i = 0 to len / 2 do
      let temp = array.(i) in
      array.(i) <- array.(len - i - 1);
      array.(len - i - 1) <- temp
    done;
    array
  in
  let f, coord, p, q = sample_grid (reverse g) n m delta stepX stepY in
  march f coord p q
