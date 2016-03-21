(*
define a simple data structure of cartesian point p= (a,b)
and tools like distance between points, lines, angle calculation,
etc
*)

type point = {x:float; y:float};;

type 'a binary_tree =
  |Empty
  |Node of 'a * 'a binary_tree * 'a binary_tree
;;
