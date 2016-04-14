(*
We define a node structure with a previous and next node
*)

(*
node:
    (data)
    (previous, data)
    (data, next)
    (previous, data, next)
*)
(*
type 'a node =                      (* à mettre dans un autre fichier *)
(*
    |single
*)
    |single of 'a
    |singleRight of 'a node *'a
    |singleLeft of 'a * 'a node
    |node of 'a node * 'a * 'a node
;;
*)
type 'a node = { single: 'a; singleRight: 'a node * 'a;
                singleLeft: 'a * 'a node; node: 'a node * 'a * 'a node}


let getData = function
    |single -> single
    |singleRight(_, d) -> d
    |singleLeft(d, _) -> d
    |node(_, d, _) -> d
;;

let hasPrevious = function
    |single(data) -> false
    |singleRight(prev, data) -> true
    |singleLeft(data, next) -> false
    |node(prev, data, next) -> true
;;

let hasNext = function
    |single(data) -> false
    |singleRight(prev, data) -> false
    |singleLeft(data, next) -> true
    |node(prev, data, next) -> true
;;

(*
let getPrevious = function
*)



let setData dt = function
    |single(data) -> single(dt)
    |singleRight(prev, data) -> singleRight(prev, dt)
    |singleLeft(data, next) -> singleLeft(dt, next)
    |node(prev, data, next) -> node(prev, dt, next)
;;

let setPrevious prev = function
    |single(data) -> singleRight(prev, data)
    |singleRight(node,data) -> singleRight(prev, data)
    |singleLeft(data, node) -> node(prev, data, node)
    |node(previous, data, next) -> node(prev, data, next)
;;

let setNext nxt = function
    |single(data) -> singleLeft(data, nxt)
    |singleRight(node, data) -> node(node, data, nxt)
    |singleLeft(data, next) -> singleLeft(data, nxt)
    |node(previous, data, next) -> node(previous, data, nxt)
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
let addEnd node = function
    Arg(head, tail, size) ->
*)


(*
let linkednode n1 n2 =
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
