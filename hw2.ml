(* HOMEWORK 2 : COMP 302 Fall 2014

   NOTE:

   All code files must be submitted electronically
   before class on Oct 2.

  The submitted file name must be hw2.ml

  Your program must type-check using OCaml.

*)

exception NotImplemented

(* -------------------------------------------------------------*)
(* QUESTION 2 :  [60 points]                                    *)
(* -------------------------------------------------------------*)

(* insert:  'a * 'b -> ('a * 'b) tree -> ('a * 'b) tree

   insert (x,d) T = T'  where (x,d) has been inserted into T
   and any previous occurrences of (x,d') in T have been
   overwritten

*)

(* let rec insert ((x,d) as e) t = match t with
  | Empty              -> Node(e, Empty, Empty)
  | Node ((y,d'), l, r) ->
      if x = y then Node(e, l, r)
      else
	(if x < y then Node((y,d'), insert e l, r)
	 else
	   Node((y,d'), l, insert e r)) *)



(* -------------------------------------------------------------*)
(* QUESTION 2.1 : [10 points]                                   *)
(* -------------------------------------------------------------*)
(* Implement a function create_tree which when given a list
   of data and key entries creates a binary tree storing all the entries, i.e.
   we repeatedly insert the entries into an empty tree.

   create_tree: ('a * 'b) list -> ('a * 'b) tree

   Use the higher-order function List.fold_left
 *)

  type 'a binary_tree =
      | Empty
      | Node of 'a * 'a binary_tree * 'a binary_tree;;

  let rec insert tree x = match tree with
      | Empty -> Node(x, Empty, Empty)
      | Node(y, l, r) ->
         if x = y then tree
         else if x < y then Node(y, insert l x, r)
         else Node(y, l, insert r x)
    let create_tree l = List.fold_left insert Empty l;;

(* -------------------------------------------------------------*)
(* QUESTION 2.2 : [10 points]                                   *)
(* -------------------------------------------------------------*)
(* Implement a function tree_map
   which when given a function f and a tree applies f to all
   the entries in the tree.
 *)

(* If you pass a function that cannot operate on the tuple, it won't work *)
(* Example of function that works assuming x is a string

    # let test (x,y) = ((2 * x),y);;
    val test : int * 'a -> int * 'a = <fun>

*)

  let rec tree_map tree f = match tree with
    | Empty -> Empty
    | Node (y, l, r) -> Node (f y, tree_map l f, tree_map r f);;

(* -------------------------------------------------------------*)
(* QUESTION 2.3 : [5 points]                                   *)
(* -------------------------------------------------------------*)
(* Delete all the data from a binary search tree which stores
   entries as pairs consisting of key and data obtaining
   a tree of the same shape.
*)
(* delete_data: ('a * 'b) tree  -> 'a tree                      *)

  let delete_data tree =
    tree_map tree (fun (x,y) -> x);;

(* -------------------------------------------------------------*)
(* QUESTION 2.4 : [15 points]                                   *)
(* -------------------------------------------------------------*)
(*  Intuitively, fold_right replaces every :: by f and nil
  by e in a list. The function tree_fold for binary trees is analogous to
  fold_right. Given a tree, tree_fold replaces each leaf by some
  value e and each node by the application of a 3-argument function  f

  It has type:

  tree_fold: ('a * 'b * 'b -> 'b) -> 'b -> 'a tree -> 'b

  Example: Given a tree
  Node (x0, Node (x1, Empty, Empty),
            Node (x2, Node (x3, Empty, Empty), Empty))

  the result will be

  f(x0, f (x1, e, e), f (x2, f (x3, e, e), e))

  15 points
 *)

   let rec tree_fold f e t = match t with
    | Empty -> e
    | Node (y, l, r) -> f y (tree_fold f e l) (tree_fold f e r);;

(* -------------------------------------------------------------*)
(* QUESTION 2.5 : [20 points]                                   *)
(* -------------------------------------------------------------*)
(* The tree_fold function allows us to express many programs which
   traverse trees elegantly in one line.

  a) Re-implement the function size : 'a tree -> int which given a
   binary tree returns the number of nodes in the tree using tree_fold
   (5 points)




  b) Implement the function reflect : 'a tree -> int which given a
   binary tree swaps the left and the right child using tree_fold
   (5 points)



  c) Implement inorder: 'a tree -> 'a list which given a binary tree
   returns a list of all entries in order.
   (10 points)

 *)

  let size_helper x y z = 1 + y + z;;

  let rec tree_fold f e t = match t with
    | Empty -> e
    | Node (y, l, r) -> f y (tree_fold f e l) (tree_fold f e r);;

  let rec size t = tree_fold size_helper 0 t;;


  let reflect_helper x y z = x y z;;

  let reflect_k = Empty;;

  let rec tree_fold_reflect f e t = match t with
    | Empty -> Empty
    | Node (y, l, r) -> Node ( y, (tree_fold_reflect f e r), (tree_fold_reflect f e l));;

  let reflect t = tree_fold_reflect reflect_helper reflect_k t;;


  let inorder_helper x y z = y @ x @ z;;

  let rec tree_fold_inorder f e t = match t with
    | Empty -> []
    | Node (y, l, r) -> f [y] (tree_fold_inorder f e l) (tree_fold_inorder f e r);;

  let inorder tree = tree_fold_inorder inorder_helper [] tree;;



(* -------------------------------------------------------------*)
(* QUESTION 3 :  [15 points]                                    *)
(* -------------------------------------------------------------*)
(* Write a function pow_series: int -> int list -> (int -> int)
   which computes the power series. Given a constant c and a
   list of coefficients a1 ... ak return a function :

   f(x) = a0 + a1*(x-c)^1 + a2*(x-c)^2 + ... + ak*(x-c)^k

   Note that the function you return should be independently
   meaningful, i.e. it should  contain only calls to pow, addition,
   subtraction or multiplication,   but no other functions.

 *)

  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n ->
      let b = pow a (n / 2) in
      b * b * (if n mod 2 = 0 then 1 else a);;

  let rec powerseriespre (c : int) l k = match l with
    | [] -> 0
    | x :: t -> if k = 0 then x + powerseriespre c t (k+1)
                else pow (x + c) k + powerseriespre c t (k+1);;

  let powerseries (c : int) l = powerseriespre c l 0;;
