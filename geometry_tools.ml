open Printf

(*
define a simple data structure of cartesian point p= (a,b)
and tools like distance between points, lines, angle calculation,
etc
*)

(*
 * point, line, distace point point, distance point line,
 * distance line line*)

type point = {x : float; y : float} (*point defined as two floats*)

let create_point a b = {x=a;y=b}

let create_point (a,b) = {x=a;y=b}

let equals p1 p2 = (p1.x=p2.x) && (p1.y=p2.y) (*equality between points*)

let is_origin p = equals p {x=0.; y=0.}
(*we define the three usual norms in the cartesian plane*)
let p_norm1 p = (abs_float p.x) +. (abs_float p.y)

let p_norm2 p = sqrt ((p.x)**2. +. (p.y)**2.)

let p_norm_infinity p = max (abs_float p.x) (abs_float p.y)

(*algebraic operations for the point type*)
let plus p1 p2 = {x=p1.x+.p2.x; y=p1.y+.p2.y}

let negative p = {x=(-.p.x); y=(-.p.y)}

let minus p1 p2 = plus p1 (negative p2)

(*induced distance by the respectives norms*)
let dist1 p1 p2 = p_norm1 (minus p1 p2)

let dist2 p1 p2 = p_norm2 (minus p1 p2)

let dist_infinity p1 p2 = p_norm_infinity (minus p1 p2)

(*test if p_test is a multiple of p_tester*)
let is_multiple p_test p_tester =
  if equals p_test p_tester then true
  else if is_origin p_test then true
  else if is_origin p_tester then false
  else if p_tester.x=0. && p_test.x!=0. then false
  else if p_tester.y=0. && p_test.y!=0. then false
  else if p_tester.x=0. then true
  else if p_tester.y=0. then true
  else (p_test.x /. p_tester.x) =(p_test.y /. p_tester.y)


(*a line type, defined by a point p_1 and a vector v,
 * the vector being a point to*)
type line = {p_1 : point; v : point} (*we use the duality between vectors
                                       and points*)

(*test for the line being a point or not*)
let line_is_degenerate li = equals li.v {x=0.; y=0.}

(*create a line from a point and a vector*)
let create_line p_point vector = {p_1= p_point; v= vector}


let point_belongs_line p li =
  if line_is_degenerate li then false
  else if p =li.p_1 then true
  else is_multiple (p -- li.p_1) li.v


let print_point p = printf "(x=%f, y=%f)" p.x p.y

let print_line li = print_string "point=";
                    print_point li.p_1;
                    print_string ", vector=";
                    print_point li.v

(*test if a point p belongs to the line li*)
let point_belongs_line p li =
  if line_is_degenerate li then false
  else if p =li.p_1 then true
  else is_multiple (minus p li.p_1) li.v

(*gives an integer to caracterize the relation between the lines
 * li1, li2, being 0 for zero points of intersection, 1 for only one
 * point of intersection and 2 otherway
 * *)
let relation_lines li1 li2 =
  if line_is_degenerate li1 && point_belongs_line li1.p_1 li2 then 1
  else if line_is_degenerate li1 && not (point_belongs_line li1.p_1 li2) then 0
  else if line_is_degenerate li2 && point_belongs_line li2.p_1 li1 then 1
  else if line_is_degenerate li2 && not (point_belongs_line li2.p_1 li1) then 0
  else if is_multiple li1.v li2.v && point_belongs_line li1.p_1 li2 then 2
  else if is_multiple li1.v li2.v && not (point_belongs_line li1.p_1 li2) then 0
  else 1
;;

let p1 = {x=0.; y=0.}
let vector = {x=1.; y=1.}
let li = {p_1=p1; v=vector}
let p = {x=2.;y=1.};;

printf "%b\n" (point_belongs_line p li);;
print_line li;;
