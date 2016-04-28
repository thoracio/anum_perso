open Geometry_tools

(*
little implementation of the flatness criterion for the curves,
we use a data structure of points p=(a,b) on the cartesian
coordinates
*)

(*the first test: given an epsilon epsi test if the sum
 * of the cords f(a)f(m) f(m)f(b) is the almost the same
 * lenght as f(a)f(b), using the distance d, where 1 is for
 * the distance 1, 2 for distance two, and 3 for the
 * infinite distance
 * *)
let cord_test a b m f epsi d =
  let p_f_a = create_point a (f a) in
  let p_f_m = create_point m (f m) in
  let p_f_b = create_point b (f b) in
  let dist = if d = 1 then dist1 else if d = 2 then dist2 else dist_infinity in
  let d_a_m = dist p_f_a p_f_m in
  let d_b_m = dist p_f_m p_f_b in
  let d_a_b = dist p_f_a p_f_b in
  abs_float (d_a_m +. d_b_m -. d_a_b) < epsi

(*return true if the distance between the point (m,fm) and
 * the line across the points (a,fa) and (b,fb) is less than
 * epsi, return false otherwise*)
let distance_point_cord_test a b m f epsi =
  let p_f_a = create_point a (f a) in
  let p_f_m = create_point m (f m) in
  let p_f_b = create_point b (f b) in
  let l_f_a_b = create_line p_f_a (minus p_f_a p_f_b) in
  (distance_line_point l_f_a_b p_f_m) < epsi

let
