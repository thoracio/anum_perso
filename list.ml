(*
We define a Node structure with a previous and next Node
*)

(*
Node:
    (data)
    (previous, data)
    (data, next)
    (previous, data, next)
*)

type 'a node =                      (* à mettre dans un autre fichier *)
(*
    |Single
*)
    |Single of 'a
    |SingleRight of 'a node *'a
    |SingleLeft of 'a * 'a node
    |Node of 'a node * 'a * 'a node
;;

(*
type 'a node = { Single: 'a; SingleRight: 'a node * 'a;
                SingleLeft: 'a * 'a Node; Node: 'a Node * 'a * 'a Node}
*)


let getData = function
    |Single(d) -> d
    |SingleRight(_, d) -> d
    |SingleLeft(d, _) -> d
    |Node(_, d, _) -> d
;;

let hasPrevious = function
    |Single(data) -> false
    |SingleRight(prev, data) -> true
    |SingleLeft(data, next) -> false
    |Node(prev, data, next) -> true
;;

let hasNext = function
    |Single(data) -> false
    |SingleRight(prev, data) -> false
    |SingleLeft(data, next) -> true
    |Node(prev, data, next) -> true
;;

(*
let getPrevious = function
*)



let setData dt = function
    |Single(data) -> Single(dt)
    |SingleRight(prev, data) -> SingleRight(prev, dt)
    |SingleLeft(data, next) -> SingleLeft(dt, next)
    |Node(prev, data, next) -> Node(prev, dt, next)
;;

let setPrevious prev = function
    |Single(data) -> SingleRight(prev, data)
    |SingleRight(node,data) -> SingleRight(prev, data)
    |SingleLeft(data, node) -> Node(prev, data, node)
    |Node(previous, data, next) -> Node(prev, data, next)
;;

let setNext nxt = function
    |Single(data) -> SingleLeft(data, nxt)
    |SingleRight(node, data) -> Node(node, data, nxt)
    |SingleLeft(data, next) -> SingleLeft(data, nxt)
    |Node(previous, data, next) -> Node(previous, data, nxt)
;;


let n1 =  Single(9.)
;;



(*
We define a doubly linked list structure for point type.
This list has a head and a tail
*)

(*
list__dl: (head, tail, size)
must be no empty
*)
type 'a list_dl =
    Arg of 'a node * 'a node * int
;;

(*
let addEnd Node = function
    Arg(head, tail, size) ->
*)


(*
let linkedNode n1 n2 =
    setPrevious n2 n1;
    setNext n1 n2
;;
*)






let getSize = function
    Arg(_, _, s) -> s
;;

let getHead = function
    Arg(hd, _, _) -> hd
;;

let getTail = function
    Arg(_, tl, _) -> tl
;;
