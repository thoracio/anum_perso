(*utilité d'un tel .mli?*)

type 'a pointer = Null | Pointer of 'a ref

val newPointer : 'a -> 'a pointer

val (!^) : 'a pointer -> 'a
