(*
define a simple data structure of cartesian point p= (a,b)
and tools like distance between points, lines, angle calculation,
etc
*)

type point = {x:float; y:float};;

(*
Compute the euclidean distance between two points
*)
let euclid_dist p q =
    sqrt((p.x -. q.x) ** 2. +. (p.y -. q.y) ** 2.)
;;

let taxi_dist p q =
    abs_float(p.x -. q.x) +. abs_float(p.y -. q.y)
;;



