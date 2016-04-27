(*

HOMEWORK 10

Name: Ben Kahle

Email: bernard.kahle@students.olin.edu

Remarks, if any:

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * Do that in a _fresh_ OCaml shell
 * It has to load without any errors.
 *
 *)


type 'a bintree =
  | Empty
  | Node of 'a * 'a bintree * 'a bintree


let sample = Node(10,Node(3,Node(7,Empty,Empty),
                            Node(5,Empty,Empty)),
                     Node(6,Node(99,Empty,
                                 Node(66,Empty,Empty)),
                          Empty))


(* Printing an integer binary tree *)

let pbt bt =
  let rec loop bt depth =
    match bt with
    | Empty -> ()
    | Node(n,left,right) ->
	(loop right (depth^"    ");
         print_endline (depth^(string_of_int n));
         loop left (depth^"    ")) in
  loop bt ""





(* Q1 *)

let rec size t =
  match t with
    | Empty -> 0
    | Node (v, l, r) -> 1 + size l + size r

let rec sum t =
  match t with
    | Empty -> 0
    | Node (v, l, r) -> v + sum l + sum r


let rec height t =
  match t with
    | Empty -> 0
    | Node (v, l, r) -> 1 + max (height l) (height r)


let rec fringe t =
  match t with
    | Empty -> []
    | Node (v, l, r) -> if (l == Empty && r == Empty) then [v] else (fringe l) @ (fringe r)


let rec map f t =
  match t with
    | Empty -> Empty
    | Node (v, l, r) -> Node (f v, map f l, map f r)


let rec fold f t b =
  match t with
    | Empty -> b
    | Node (v, l, r) -> f v (fold f l b) (fold f r b)

let preorder t = fold (fun v l r -> [v] @ l @ r) t []


let postorder t = fold (fun v l r -> l @ r @ [v]) t []


let inorder t = fold (fun v l r -> l @ [v] @ r) t []


let rec bst_insert t x =
  match t with
    | Empty -> Node (x, Empty, Empty)
    | Node (v, l, r) -> if (v > x) then Node (v, bst_insert l x, r) else Node (v, l, bst_insert r x)


let rec bst_lookup t x =
  match t with
    | Empty -> false
    | Node (v, l, r) -> if (v = x) then true else (if (v > x) then bst_lookup l x else bst_lookup r x)


let rec bstify t =
  List.fold_left (fun t v -> bst_insert t v) Empty (preorder t)


let avl_insert t x = failwith ("avl_insert not implemented")
