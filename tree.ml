(*
We define an avl binary tree strucuture for the float type:
(right_subtree*data*left_subtree*height).
Order preserving, all the elements at the left smaller
than the root, all greater than the root to the right.
*)

type 'a avlbst =
  |Leaf
  |Node of 'a avlbst * 'a * 'a avlbst * int
;;

(*
returns an integer as the height of the tree
in constant time
*)
let height = function
  |Leaf -> 0
  |Node(_, _, _, h) -> h
;;

(*
returns an avl tree, the left subtree
*)
let getLeft = function
  |Leaf -> Leaf
  |Node(left, _, _, _) -> left
;;

(*
returns an avl tree, the rigth subtree
*)
let getRigth = function
  |Leaf -> Leaf
  |Node(_, _, rigth, _) -> rigth
;;

(*
sets new_left as the left subtree.
If no tree exists, new_left become the tree
*)
let setLeft new_left = function
  |Leaf -> new_left
  |Node(left, root, rigth, h) ->
    Node(new_left, root, rigth, (max (height new_left) (height rigth)))
;;

(*
sets new_rigth as the rigth subtree.
If no tree exists, new_rigth become the tree
*)
let setRigth new_rigth = function
  |Leaf -> new_rigth
  |Node(left, root, rigth, h) ->
    Node(left, root, new_rigth, (max (height left) (height new_rigth)))
;;

(*
left rotation for the tree in constant time
*)
let lRotation = function
  |Leaf -> Leaf
  |Node(left, root, rigth, h) ->
    match rigth with  (*it depends if the rigth subtree exists or not*)
      |Leaf -> Node(left, root, rigth, h) (*nothing to do*)
      |Node(r_left, r_root, r_rigth, r_h) ->
        let max_left_r_left = max (height left) (height r_left) in (*calculate the heigth of the new
                                                                    left subtree*)
        let max_new_h = max (1+ max_left_r_left) (height r_rigth) in (*calculate the heigth of the
                                                                        new tree*)
        Node(Node(left, root, r_left, 1 + max_left_r_left), r_root, r_rigth, max_new_h)
;;

(*
rigth rotation for the tree in constant time
*)
let rRotation = function
  |Leaf -> Leaf
  |Node(left, root, rigth, h) ->
    match left with  (*it depends if the left subtree exists or not*)
      |Leaf -> Node(left, root, rigth, h) (*nothing to do*)
      |Node(l_left, l_root, l_rigth, l_h) ->
        let new_rigth_l_rigth_h = 1 + (max (height rigth) (height l_rigth)) in (*calculate the heigth of the new
                                                                    rigth subtree*)
        let new_h = max new_rigth_l_rigth_h (height l_left) in (*calculate the heigth of the
                                                                        new tree*)
        Node(l_left, l_root, Node(l_rigth, root, left, new_rigth_l_rigth_h), new_h)
;;

(*
left-rigth rotation in constant time
*)
let lrRotation = function
  |Leaf -> Leaf
  |Node(left, root, rigth, h) ->
    if (height (getLeft rigth)) == 0 then Node(left, root, rigth, h) (*we assure the left-rigth
                                                                      subtree exists*)
    else(
      let Node(Node(rl_left, rl_root, rl_rigth, rl_h), r_root, r_rigth, r_h) = rigth in (*pattern matching*)
      let new_l_h = 1 + (max (height left) (height rl_left)) in
      let new_r_h = 1 + (max (height rl_rigth) (height r_rigth)) in
      let new_h = 1 + (max new_l_h new_r_h) in
      Node(Node(left, root, rl_left, new_l_h), rl_root, Node(rl_rigth, r_root, r_rigth, new_r_h), new_h)
      )
;;
