type 'a pointer =
    |Null
    |Pointer of 'a ref;;

let newPointer my_object =
    Pointer(ref my_object)

let (!^) = function
    |Null -> invalid_arg "Attempt to dereference the null pointer"
    |Pointer r -> !r
