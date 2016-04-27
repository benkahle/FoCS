(*

HOMEWORK 9

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



(* The underlying implementation for streams
 *
 * Basically, a stream is a pair of an element and a
 * "promise" to compute the rest of the stream.
 *
 * That "promise" is represented as a function
 *
 * The implementation memoizes that function: once the function is
 * called once, it remembers its result, and subsequent calls to the
 * function directly return the result without executing the body of
 * the function
 *
 * You don't need to know anything about this code -- you will
 * use functions fby, head, and tail described below instead
 *)

module AbsStream :
  sig
      type 'a stream
      val mk : 'a -> (unit -> 'a stream) -> 'a stream
      val unmk1 : 'a stream -> 'a
      val unmk2 : 'a stream -> 'a stream
      val cst : 'a -> 'a stream
      val fby : 'a stream -> (unit -> 'a stream) -> 'a stream
      val map : ('a -> 'b) -> 'a stream -> 'b stream
      val filter : ('a -> 'b -> bool) -> 'a stream -> 'b stream -> 'b stream
      val split : 'a stream -> ('a stream * 'a stream)
      val zip : 'a stream -> 'b stream -> ('a * 'b) stream
      val prefix : int -> 'a stream -> 'a list
      val nth : int -> 'a stream -> 'a
    end =
  struct

    type 'a stream = R of 'a * (unit -> 'a stream)

    let memoize f =
      let memoized = ref None in
      let new_f () =
	match !memoized with
	| None -> let result = f () in memoized := Some result; result
	| Some v -> v   in
      new_f

    let mk h t = R (h, memoize t)
    let unmk1 s = let R (h,t) = s in h
    let unmk2 s = let R (h,t) = s in t ()

    let rec cst v = mk v (fun () -> cst v)
    let fby s1 ps2 = mk (unmk1 s1) ps2
    let rec map f s = mk (f (unmk1 s)) (fun () -> map f (unmk2 s))
    let rec filter p ctl s = if p (unmk1 ctl) (unmk1 s) then mk (unmk1 s) (fun () -> filter p (unmk2 ctl) (unmk2 s)) else filter p (unmk2 ctl) (unmk2 s)
    let split s = (cst (unmk1 s), unmk2 s)
    let rec zip s1 s2 = mk (unmk1 s1, unmk1 s2) (fun () -> zip (unmk2 s1) (unmk2 s2))

    let rec prefix n s = if n > 0 then (unmk1 s)::(prefix (n-1) (unmk2 s)) else []
    let rec nth n s = if n > 0 then nth (n-1) (unmk2 s) else unmk1 s

  end


(*
 * These are the stream functions you will use
 *
 *)

type 'a stream = 'a AbsStream.stream

let cst : 'a -> 'a stream = AbsStream.cst
        (* constant *)

let fby : 'a stream -> (unit -> 'a stream) -> 'a stream = AbsStream.fby
        (* followed by *)

let map : ('a -> 'b) -> 'a stream -> 'b stream = AbsStream.map
        (* map a function over a stream *)

let filter : ('a -> 'b -> bool) -> 'a stream -> 'b stream -> 'b stream = AbsStream.filter
           (* filter a stream based on a control stream and a predicate *)

let zip : 'a stream -> 'b stream -> ('a * 'b) stream = AbsStream.zip
        (* zip two streams into a stream of pairs *)

let split : 'a stream -> ('a stream * 'a stream) = AbsStream.split
          (* split a stream into two streams *)

let prefix : int -> 'a stream -> 'a list = AbsStream.prefix
           (* return the first n elements of a stream *)

let nth : int -> 'a stream -> 'a = AbsStream.nth
           (* return the nth element of a stream *)



(* some useful sample streams, from class *)

let nats =
  let rec natsF () = fby (cst 0)
                         (fun () -> (map (fun x -> x+1) (natsF ()))) in
  natsF ()

let evens = map (fun x -> 2*x) nats
let odds = map (fun x -> x+1) evens



(* this one is cute *)

let ampl =
  let transf (v,(d,m)) =
    if d = 1 && v = m then (v-1,(-1,m))
    else if d = -1 && v = -m then (v+1,(1,m+1))
    else if d = 1 then (v+1,(1,m))
    else (v-1,(-1,m))  in
  let rec f () = fby (zip (cst 0) (cst (1,1)))
                     (fun () -> map transf (f ())) in
  map (fun (x,y) -> x) (f ())


(* streams of the form a0,a1,a2,a3... that are useful for
   illustrating question 3 *)

let tag tg =
  map (fun (t,n) -> t^(string_of_int n)) (zip (cst tg) nats)

let s_a = tag "a"
let s_b = tag "b"


let drop s = let (f,r) = split s in r
(*
 * QUESTION 1
 *
 *)

let scale n s = map (fun x -> n*x) s

let mult s1 s2 = map (fun (a,b) -> a*b) (zip s1 s2)

let unzip s = (map (fun (a, b) -> a) s, map (fun (a, b) -> b) s)

let rec fold f init_s s =
  let rec foldF () = fby (init_s) (fun () -> (map (fun (x, i) -> f x i) (zip s (foldF ())))) in
  drop (foldF ())

let running_max s = fold (fun v l -> if v > l then v else l) (cst 0) s

let rec stutter s = fby (s) (fun () -> (fby (s) (fun () -> (stutter (drop s)))))


let natsf =
  let rec natsfF () = fby (cst 0.) (fun () -> (map (fun x -> x+.1.) (natsfF ()))) in
    natsfF()
let evensf = map (fun x -> 2.*.x) natsf
let oddsf = map (fun x -> x+.1.) evensf
let scalef n s = map (fun x -> n*.x) s
let addf s1 s2 = map (fun (a, b) -> a+.b) (zip s1 s2)
let rec psums s = fby (s) (fun () -> map (fun (a,b) -> a+b) (zip (psums s) (drop s)))
let rec psumsf s = fby (s) (fun () -> map (fun (a,b) -> a+.b) (zip (psumsf s) (drop s)))
(*
 * QUESTION 2
 *
 *)

let rec arctan z = psumsf (map (fun ((x, odd), nat) -> if (nat mod 2 = 0) then (x**odd)/.odd else -.(x**odd)/.odd) (zip (zip (cst z) oddsf) nats))

(* PLACEHOLDER -- REPLACE WITH YOUR OWN DEFINITION *)

let pi = addf (scalef 16. (arctan (1./.5.))) (scalef (-.4.) (arctan (1./.239.)))

let rec newton f df guess = fby (cst guess)
  ( fun () ->
    let gs = (newton f df guess) in
    addf (gs) (scalef (-1.0) (map (fun (n,d) -> n/.d) (zip (map f gs) (map df gs))))
  )

let derivative f x = map (fun n -> ((f (x +. 1./.n)) -. (f x))/.(1./.n)) (drop natsf)

let limit epsilon s = filter (fun a b -> if abs_float (a-.b) < epsilon then true else false) (drop s) s


(*
 * QUESTION 3
 *
 *)


let rec rev_prefixes s = fby (map (fun a -> [a]) s) (fun () -> map (fun (a,b) -> a::b) (zip (drop s) (rev_prefixes s)))

let rec prefixes s = fby (map (fun a -> [a]) s) (fun () -> map (fun (a,b) -> b@[a]) (zip (drop s) (prefixes s)))

let stripes s1 s2 = map (fun (la, lb) -> List.map2 (fun a b -> (a,b)) la lb) (zip (prefixes s1) (rev_prefixes s2))

let first s = let (f,r) = split s in f

let rec flatten ss =
  let nonEmpties = (filter (fun a b -> if a = [] then false else true) ss ss) in
  fby (map (fun l -> List.hd l) nonEmpties)
  (fun () -> flatten (
      fby (map (fun l -> List.tl l) nonEmpties)
      (fun () -> drop nonEmpties)
    )
  )

let pairs s1 s2 =  flatten (stripes s1 s2)
