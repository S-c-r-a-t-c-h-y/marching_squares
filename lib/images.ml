type ppm_pixels =
  | P1 of int array array
  | P2 of int array array
  | P3 of (int * int * int) array array

type ppm_image = {
  width : int;
  height : int;
  max_value : int;
  pixels : ppm_pixels;
}

type rgb_image = {
  width : int;
  height : int;
  pixels : (int * int * int) array array;
}

let to_gray_scale arr =
  Array.map (fun x -> Array.map (fun (r, g, b) -> (r + b + g) / 3) x) arr

let ppm_to_rgb (ppm : ppm_image) =
  match ppm.pixels with
  | P1 pixels ->
      {
        width = ppm.width;
        height = ppm.height;
        pixels =
          Array.map
            (Array.map (fun x -> if x = 1 then (255, 255, 255) else (0, 0, 0)))
            pixels;
      }
  | P2 pixels ->
      {
        width = ppm.width;
        height = ppm.height;
        pixels =
          Array.map
            (Array.map (fun x ->
                 let v = x * 255 / ppm.max_value in
                 (v, v, v)))
            pixels;
      }
  | P3 pixels ->
      {
        width = ppm.width;
        height = ppm.height;
        pixels =
          Array.map
            (Array.map (fun (r, g, b) ->
                 ( r * 255 / ppm.max_value,
                   g * 255 / ppm.max_value,
                   b * 255 / ppm.max_value )))
            pixels;
      }

let process_p1 ic =
  let get_dimension ic =
    match
      input_line ic |> String.split_on_char ' ' |> List.map int_of_string
    with
    | [] | [ _ ] -> failwith "invalid input"
    | dimx :: dimy :: _ -> (dimx, dimy)
  in
  let dimx, dimy = get_dimension ic in
  let img = Array.init dimy (fun _ -> [||]) in
  for i = 0 to dimy - 1 do
    img.(i) <-
      input_line ic |> String.split_on_char ' ' |> List.map int_of_string
      |> Array.of_list
  done;
  { width = dimx; height = dimy; max_value = 1; pixels = P1 img }

let process_p2 ic =
  let get_dimension ic =
    match
      input_line ic |> String.split_on_char ' ' |> List.map int_of_string
    with
    | [] | [ _ ] -> failwith "invalid input"
    | dimx :: dimy :: _ -> (dimx, dimy)
  in
  let dimx, dimy = get_dimension ic in
  let max_val = input_line ic |> int_of_string in
  let img = Array.init dimy (fun _ -> [||]) in
  for i = 0 to dimy - 1 do
    img.(i) <-
      input_line ic |> String.split_on_char ' ' |> List.map int_of_string
      |> Array.of_list
  done;
  { width = dimx; height = dimy; max_value = max_val; pixels = P2 img }

let process_p3 ic =
  let get_dimension ic =
    match
      input_line ic |> String.split_on_char ' ' |> List.map int_of_string
    with
    | [] | [ _ ] -> failwith "invalid input"
    | dimx :: dimy :: _ -> (dimx, dimy)
  in
  let dimx, dimy = get_dimension ic in
  let max_val = input_line ic |> int_of_string in
  let img = Array.init dimy (fun _ -> Array.init dimx (fun _ -> (0, 0, 0))) in
  for i = 0 to dimy - 1 do
    input_line ic |> ignore;
    for j = 0 to dimx - 1 do
      match
        input_line ic |> String.split_on_char ' ' |> List.map int_of_string
      with
      | r :: g :: b :: _ -> img.(i).(j) <- (r, g, b)
      | _ -> failwith "invalid input"
    done
  done;
  { width = dimx; height = dimy; max_value = max_val; pixels = P3 img }

let open_ppm img =
  let ic = open_in img in
  match input_line ic with
  | "P1" -> process_p1 ic
  | "P2" -> process_p2 ic
  | "P3" -> process_p3 ic
  | _ -> failwith "not implemented"
