type 'a listDl

val newListDl : unit -> 'a listDl

val getHead : 'a listDl -> 'a Node.node

val getTail : 'a listDl -> 'a Node.node

val getSize : 'a listDl -> int

val addHead : 'a listDl -> 'a Node.node -> unit

val addTail : 'a listDl -> 'a Node.node -> unit

val (@@) : 'a listDl -> 'a listDl -> 'a listDl