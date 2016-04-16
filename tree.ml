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
returns an avl tree, the right subtree
*)
let getRight = function
  |Leaf -> Leaf
  |Node(_, _, right, _) -> right
;;

(*
sets new_left as the left subtree.
If no tree exists, new_left become the tree
*)
let setLeft new_left = function
  |Leaf -> new_left
  |Node(left, root, right, h) ->
    Node(new_left, root, right, (max (height new_left) (height right)))
;;

(*
sets new_right as the right subtree.
If no tree exists, new_right become the tree
*)
let setRight new_right = function
  |Leaf -> new_right
  |Node(left, root, right, h) ->
    Node(left, root, new_right, (max (height left) (height new_right)))
;;

(*
left rotation for the tree in constant time
*)
let lRotation = function
  |Leaf -> Leaf
  |Node(left, root, right, h) ->
    match right with  (*it depends if the right subtree exists or not*)
      |Leaf -> Node(left, root, right, h) (*nothing to do*)
      |Node(r_left, r_root, r_right, r_h) ->
        let max_left_r_left = max (height left) (height r_left) in (*calculate the heigth of the new
                                                                    left subtree*)
        let max_new_h = max (1+ max_left_r_left) (height r_right) in (*calculate the heigth of the
                                                                        new tree*)
        Node(Node(left, root, r_left, 1 + max_left_r_left), r_root, r_right, max_new_h)
;;

(*
right rotation for the tree in constant time
*)
let rRotation = function
  |Leaf -> Leaf
  |Node(left, root, right, h) ->
    match left with  (*it depends if the left subtree exists or not*)
      |Leaf -> Node(left, root, right, h) (*nothing to do*)
      |Node(l_left, l_root, l_right, l_h) ->
        let new_right_l_right_h = 1 + (max (height right) (height l_right)) in (*calculate the heigth of the new
                                                                    right subtree*)
        let new_h = max new_right_l_right_h (height l_left) in (*calculate the heigth of the
                                                                        new tree*)
        Node(l_left, l_root, Node(l_right, root, left, new_right_l_right_h), new_h)
;;

(*
left-right rotation in constant time
*)
let lrRotation = function
  |Leaf -> Leaf
  |Node(left, root, right, h) ->
    if (height (getLeft right)) == 0 then Node(left, root, right, h) (*we assure the left-right
                                                                      subtree exists*)
    else(
      let Node(Node(rl_left, rl_root, rl_right, rl_h), r_root, r_right, r_h) = right in (*pattern matching*)
      let new_l_h = 1 + (max (height left) (height rl_left)) in
      let new_r_h = 1 + (max (height rl_right) (height r_right)) in
      let new_h = 1 + (max new_l_h new_r_h) in
      Node(Node(left, root, rl_left, new_l_h), rl_root, Node(rl_right, r_root, r_right, new_r_h), new_h)
      )
;;
