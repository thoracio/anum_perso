(*
We define a doubly linked list structure for point type.
This list has a head and a tail
*)

(*
node
*)
type 'a node = Data | Previous | Next (* à mettre dans un autre fichier *)
(*
type 'a node = Node of 'a * 'a node * 'a node
*)
;;

(*
list__dl: (head, tail, size)
*)
type 'a list_dl =
    Arg of 'a node * 'a node * int
;;
(*
type 'a list_dl = Head | Tail | Size
;;
*)


let size = function
    Arg(_, _, s) -> s
;;

let getHead = function
    Arg(hd, _, _) -> hd
;;

let getTail = function
    Arg(_, tl, _) -> tl
