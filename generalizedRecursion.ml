let rec squares (li) =
  match li with
    | [] -> []
    | h::t -> (h*h)::squares(t)

let rec diags (li) =
  match li with
    | [] -> []
    | h::t -> (h,h)::diags(t)

let rec lengths (li) =
  match li with
    | [] -> []
    | h::t -> List.length(h)::lengths(t)

let rec map f xs =
  match xs with
    | [] -> []
    | x::xs' -> (f x)::map f xs'

let rec map_append f xs =
  match xs with
    | [] -> []
    | x::xs' -> (f x) @ (map_append f xs')

let diags3 xs = map (fun x -> (x,x,x)) xs

let thirds xs = map (fun (a,b,c) -> c) xs

let distribute e xs = map (fun x -> (e, x)) xs

let create_mkPair y = (fun x -> (y,x))

let create_mkPair y x = (y,x)

let distribute y xs =
  map (create_mkPair y) xs


let rec removeEmpty xss =
  match xss with
    | [] -> []
    | xs::xss' -> if (List.length xs > 0) then xs::(removeEmpty xss') else (removeEmpty xss')

let rec filter p xs =
  match xs with
    | [] -> []
    | x::xs' -> if (p x) then x::(filter p xs') else (filter p xs')

let flatten xss = map_append (fun xs -> xs) xss

let filter p xs = map_append (fun x -> if (p x) then [x] else []) xs

let rec map_general1 comb f xs =
  match xs with
    | [] -> []
    | x::xs' -> comb (f x) (map_general1 comb f xs')

let map f xs = map_general1 (fun x y -> x::y) f xs
let map_append f xs = map_general1 (fun x y -> x@y) f xs

let rec map_general comb xs =
  match xs with
    | [] -> []
    | x::xs' -> comb x (map_general comb xs')

let map f xs = map_general (fun x y -> (f x)::y) xs
let map_append f xs = map_general (fun x y -> (f x)@y) xs

let rec sum xs =
  match xs with
    | [] => 0
    | x::xs' -> x + sum xs'

let rec sum_general comb xs =
  match xs with
    | [] -> 0
    | x::xs' -> comb x (sum_general comb xs')

let sum xs = sum_general (fun x y -> x+y) xs

let rec fold_right comb xs base = (* AKA reduce *)
  match xs with
    | [] -> base
    | x::xs' -> comb x (fold_right comb xs')
