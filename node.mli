type 'a node

val newNode : 'a -> 'a node

val getData : 'a node -> 'a

val getPrevious : 'a node -> 'a node

val getNext : 'a node -> 'a node

val setData : 'a node -> 'a -> unit

val link : 'a node -> 'a node -> unit