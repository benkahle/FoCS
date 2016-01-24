(*

HOMEWORK 1

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
 * It has to load without any errors.
 *
 *)



(* Question 1 *)

let rec gcd (a,b) =
  if a > b then
    if b = 0 then a
    else gcd(a-b, b)
  else
    if a = 0 then b
    else gcd(a, b-a)

let is_coprime (a,b) =
  if gcd(a,b) = 1 then true else false

let euler (n) =
  let rec eulerHelper (a, b, value) =
    let nextValue = if is_coprime(a, b) = true then value + 1 else value in
    if b = 1 then nextValue else eulerHelper(a, b-1, nextValue)
  in
  eulerHelper(n, n, 0)

let coprimes (n) =
  let rec coprimesHelper (a, b, coprimes) =
    let nextCoprimes = if is_coprime(a, b) = true then b :: coprimes else coprimes in
    if b = 1 then nextCoprimes else coprimesHelper(a, b-1, nextCoprimes)
  in
  coprimesHelper(n, n, [])


(* Question 2 *)

let rec append (xs,ys) =
  match xs with
    | [] -> ys
    | head :: tail -> head :: append(tail, ys)


let rec flatten (xss) =
  match xss with
    | [] -> []
    | head :: tail -> append(head, flatten(tail))


let rec nth (n,xs) =
  if n = 0 then
    match xs with
      | [] -> failwith "out of bounds"
      | head :: tail -> head
  else
    match xs with
      | [] -> failwith "out of bounds"
      | head :: tail -> nth(n-1, tail)


let rec last (xs) =
  match xs with
    | [] -> failwith "empty list"
    | [x] -> x
    | head :: tail -> last(tail)


let separate (xs) =
  let rec separateHelper (firsts, seconds, xs) =
    match xs with
      | [] -> (firsts, seconds)
      | head :: tail ->
        match head with
          | a, b -> separateHelper(append(firsts,[a]), append(seconds,[b]), tail)
  in
  separateHelper([],[],xs)


(* Question 3 *)

let rec setIn (e,xs) =
  match xs with
    | [] -> false
    | head :: tail -> if head = e then true else setIn(e, tail)


let rec setSub (xs,ys) =
  match xs with
    | [] -> true
    | head :: tail -> if setIn(head, ys) = true then setSub(tail, ys) else false


let setEqual (xs,ys) =
  if setSub(xs, ys) = true && setSub(ys, xs) = true then true else false


let setUnion (xs,ys) =
  let rec removeDuplicates(xs, final) =
    match xs with
      | [] -> final
      | head :: tail ->
        if setIn(head, final) = false then
          removeDuplicates(tail, append(final, [head]))
          (* head :: final would be quicker but I want to maintain order *)
        else removeDuplicates(tail, final)
  in
  removeDuplicates(append(xs,ys), [])
  (* append on its own would work if I didn't want to remove duplicates *)


let setInter (xs,ys) =
  let rec intersectHelper(xs, ys, zs, final) =
    match zs with
      | [] -> final
      | head :: tail ->
        if setIn(head, xs) = true && setIn(head, ys) = true && setIn(head, final) = false then
          intersectHelper(xs, ys, tail, append(final, [head]))
        else
          intersectHelper(xs, ys, tail, final)
  in
  intersectHelper(xs, ys, append(xs,ys), [])


let setSize (xs) =
  let rec length(ys, count) =
    match ys with
      | [] -> count
      | head :: tail -> length(tail, count+1)
  in
  length(setUnion(xs,[]), 0)
