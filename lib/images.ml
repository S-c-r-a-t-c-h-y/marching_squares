exception Corrupted_image of string

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

(* let inverse ppm = {width=ppm.width; height=ppm.height; max_value=ppm.max_value; pixels = } *)

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

let parse_ppm ic : ppm_image =
  let scanner = Scanf.Scanning.from_function (fun () -> input_char ic) in
  let rec pass_comments () =
    try
      Scanf.bscanf scanner "#%[^\n\r]%[\t\n\r]" (fun _ _ -> ());
      pass_comments ()
    with _ -> ()
  in

  (* HEADER PARSING *)
  let magic = ref "" in
  let width = ref (-1) and height = ref (-1) in
  let max_val = ref 1 in
  pass_comments ();
  Scanf.bscanf scanner "%s%[\t\n ]" (fun mn _ -> magic := mn);
  pass_comments ();

  (try Scanf.bscanf scanner "%u%[\t\n ]" (fun w _ -> width := w)
   with Stdlib.Scanf.Scan_failure _ ->
     raise (Corrupted_image "PPM: invalid width"));

  (try Scanf.bscanf scanner "%u%[\t\n ]" (fun h _ -> height := h)
   with Stdlib.Scanf.Scan_failure _ ->
     raise (Corrupted_image "PPM: invalid height"));

  (if List.mem !magic [ "P2"; "P3" ] then
     try Scanf.bscanf scanner "%u%1[\t\n ]" (fun mv _ -> max_val := mv)
     with Stdlib.Scanf.Scan_failure _ ->
       raise (Corrupted_image "PPM: invalid max_val"));

  (* CONTENT PARSING *)
  match !magic with
  | "P1" | "P2" ->
      let pixels =
        Array.init !height (fun _ -> Array.init !width (fun _ -> 0))
      in
      for y = 0 to !height - 1 do
        for x = 0 to !width - 1 do
          try Scanf.bscanf scanner "%d%[\t\n ]" (fun v _ -> pixels.(y).(x) <- v)
          with Stdlib.Scanf.Scan_failure _ ->
            raise (Corrupted_image "PPM: Invalid grayscale pixel data")
        done
      done;
      if !magic = "P1" then
        {
          width = !width;
          height = !height;
          max_value = !max_val;
          pixels = P1 pixels;
        }
      else
        {
          width = !width;
          height = !height;
          max_value = !max_val;
          pixels = P2 pixels;
        }
  | "P3" ->
      let pixels =
        Array.init !height (fun _ -> Array.init !width (fun _ -> (0, 0, 0)))
      in
      for y = 0 to !height - 1 do
        for x = 0 to !width - 1 do
          Scanf.bscanf scanner "%d%[\t\n ]%d%[\t\n ]%d%[\t\n ]"
            (fun r _ g _ b _ -> pixels.(y).(x) <- (r, g, b))
        done
      done;
      {
        width = !width;
        height = !height;
        max_value = !max_val;
        pixels = P3 pixels;
      }
  | _ -> raise (Corrupted_image "Invalid PPM format")

let open_ppm img =
  let ic = open_in img in
  parse_ppm ic
