open Pointer
open Node

(*
We define a doubly linked list structure for point type.
*)

(*
listDl: (head, tail, size)
a listDl handles pointer type and not directly node type
a listDl can't contain two same physical nodes
*)

type 'a listDl = {mutable head: 'a Node.node Pointer.pointer;
                  mutable tail: 'a Node.node Pointer.pointer;
                  mutable size: int}

let newListDl () = {head = Null; tail = Null; size = 0}



let isEmpty list = list.size = 0



let getHead list = !^(list.head)

let getTail list = !^(list.tail)

let getSize list = list.size


let firstAdd list node =
    list.head <- Pointer (ref node);
    list.tail <- Pointer (ref node);
    list.size <- 1

let addHead list node =
    if isEmpty list then firstAdd list node
    else
    (
        link node (getHead list);
        list.head <- Pointer (ref node);
        list.size <- getSize list + 1
    )

let addTail list node =
    if isEmpty list then firstAdd list node
    else
    (
    link (getTail list) node;
    list.tail <- Pointer (ref node);
    list.size <- getSize list + 1
    )




(*
concatenation of two listDl
*)
let (@@) l1 l2 =
    link (getTail l1) (getHead l2);
    {head = l1.head; tail = l2.tail; size = l1.size + l2.size}



(*
the following methods serve to test
to delete
*)

let rec printListDlRec current tail =
    print_float (getData current);
    if current == tail then ()
    else(
        print_string " -- ";
        printListDlRec (getNext current) tail
        )


let printListDl list =
    print_string ("printed list of " ^ string_of_int(list.size) ^ " node(s): ");
    if isEmpty list then ()
    else printListDlRec (getHead list) (getTail list);
    print_newline();;



let n1 = newNode 1.
let n2 = newNode 2.
let n3 = newNode 3.
let n4 = newNode 4.
let n5 = newNode 5.
let n6 = newNode 6.
let n7 = newNode 7.
let n8 = newNode 8.;;


print_newline();;


let l1 = newListDl ();;
printListDl l1;;

addTail l1 n1;;
printListDl l1;;

addHead l1 n2;;
printListDl l1;;

addTail l1 n3;;
printListDl l1;;

addHead l1 n4;;
printListDl l1;;



print_newline();;


let l2 = newListDl ();;
printListDl l2;;

addTail l2 n5;;
printListDl l2;;

addHead l2 n6;;
printListDl l2;;

addTail l2 n7;;
printListDl l2;;

addHead l2 n8;;
printListDl l2;;


let l3 = l1 @@ l2;;
let l4 = l2 @@ l1;;

print_newline();;

printListDl l3;;

print_newline();;

printListDl l4;;



print_int(getSize l1);;print_newline();;
print_int(getSize l2);;print_newline();;
print_int(getSize l3);;print_newline();;
print_int(getSize l4);;print_newline();;


let l5 = l3 @@ l4;;

printListDl l5;;


print_int(getSize l5);;print_newline();;



