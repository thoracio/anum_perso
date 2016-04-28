open Pointer
(*
We define a node structure with a previous and next node
*)

(*
Node:
    (data)
    (previous, data)
    (data, next)
    (previous, data, next)
*)



type 'a node = {mutable data: 'a;
                mutable left_node: 'a node Pointer.pointer;
                mutable right_node: 'a node Pointer.pointer}

let newNode dt = {data = dt; left_node = Null; right_node = Null}


let getData n =
    n.data

let getPrevious n =
    !^(n.left_node)

let getNext n =
    !^(n.right_node)

let setData n dt =
    n.data <- dt

let setPrevious nd prev_nd =
    nd.left_node <- newPointer prev_nd

let setNext nd next_nd =
    nd.right_node <- newPointer next_nd


let link n1 n2 =
    setNext n1 n2;
    setPrevious n2 n1

let insert n_center n_prev n_next =
    link n_prev n_center;
    link n_center n_next


let hasPrevious n =
    n.left_node = Null

let hasNext n =
    n.right_node = Null


let printNode node =
    print_endline(string_of_float(node.data))

