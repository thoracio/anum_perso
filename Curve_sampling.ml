
(*
adaptative sampling for the function f to the interval [a,b]
*)
let main_adaptative_sampling f a b =
    if


(*
uniform sampling for the function f to the interval [a,b] with n points
*)
let main_uniform_sampling f a b n =
    let data_structure = new_data_structure in
    if n < 1 then failwith"this number is smaller than 1"
    else if n <> 1 then
        let m = n - 1 in
        let h = (b -. a) /. m in
        for i = 0 to m do
            data_structure.add (create_point (a +. h *. float_of_int i)
                                                (f(a +. h *. float_of_int i)))
        done;
        data_structure
    else(
        data_structure.add (create_point ((a +. b) /. 2)
                                                (f((a +. b) /. 2)));
        data_structure
    )