(*
   INPUT : G : matrice m * n de scalaires
           stepX, stepY la résolution d'échantilonage
           delta une isovaleur

   OUTPUT : un set gamma de lignes de contours
*)

open Marching_squares

(* let test_cloud =
     [|
       [| 255; 255; 255; 255; 255 |];
       [| 255; 255; 182; 255; 255 |];
       [| 255; 182; 182; 182; 255 |];
       [| 255; 182; 182; 182; 255 |];
       [| 255; 255; 255; 255; 255 |];
     |]
   ;; *)

(* Display.march_and_draw test_cloud 5 5 190 1 1 200 *)

let () =
  (* let dimx, dimy, _, img = Images.open_ppm "src/test.pnm" in
     Display.march_and_draw img dimy dimx 0 1 1 50 *)
  (* let dimx, dimy, max_val, img = Images.open_ppm "src/test2.pnm" in
     Display.march_and_draw img dimy dimx (max_val / 2) 1 1 200 *)
  (* let dimx, dimy, _, img = Images.open_ppm "src/test_cloud.pnm" in
     Display.march_and_draw img dimy dimx 190 1 1 200 *)
  (* let ppm = Images.open_ppm "src/test_cloud.pnm" in
     let rgb_img = Images.ppm_to_rgb ppm in
     Display.display_rgb_image rgb_img 0 0 ~scale:100 *)
  (* let ppm = Images.open_ppm "src/test2.pnm" in
     let rgb_img = Images.ppm_to_rgb ppm in
      Display.display_rgb_image rgb_img 0 0 ~scale:100 *)
  (* let ppm = Images.open_ppm "src/test.pnm" in
     let rgb_img = Images.ppm_to_rgb ppm in
     Display.display_rgb_image rgb_img 0 0 ~scale:100 *)
  let ppm = Images.open_ppm "src/champollion_cour_honneur.pnm" in
  let rgb_img = Images.ppm_to_rgb ppm in
  Display.display_rgb_image rgb_img 0 0 ~scale:1
(* Display.march_and_draw_ppm "src/test_cloud.pnm" 190 1 1 200 *)
