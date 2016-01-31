(*

HOMEWORK 2

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



(* QUESTION 1 *)


let prepend (letter, lang) =
  let rec prependHelper (a, bs, final) =
    match bs with
      | [] -> final
      | head :: tail -> prependHelper(a, tail, (a^head) :: final)
  in
  prependHelper(letter, lang, [])

let rec append (xs,ys) =
  match xs with
    | [] -> ys
    | head :: tail -> head :: append(tail, ys)

let concatenate (alphabet, lang) =
  let rec cHelper (ss, bs, final) =
    match ss with
      | [] -> final
      | head :: tail -> cHelper(tail, bs, append(prepend(head, bs), final))
  in
  cHelper(alphabet, lang, [])

let all_strings (alphabet, n) =
  let rec allStringsUpTo (alphabet, lastAddedLang, lang, i) =
    if i > n then
      "" :: lang
    else
      let newLang = concatenate(alphabet, lastAddedLang) in
      allStringsUpTo(alphabet, newLang, append(newLang, lang), i+1)
  in
  allStringsUpTo(alphabet, [""], [], 1)



(* QUESTION 2 *)

let restrict (xs,n) =
  let rec removeIfTooLong (xs, accum, maxLength) =
    match xs with
      | [] -> accum
      | head :: tail ->
        if String.length(head) > maxLength then
          removeIfTooLong(tail, accum, maxLength)
        else
          removeIfTooLong(tail, head :: accum, maxLength)
  in
  removeIfTooLong(xs, [], n)

(* Shameless reuse from hw1 *)
let rec setIn (e,xs) =
  match xs with
    | [] -> false
    | head :: tail -> if head = e then true else setIn(e, tail)

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


let langUnion (xs,ys,n) =
  setUnion(restrict(xs, n), restrict(ys, n))


let langConcat (xs,ys,n) =
  restrict(concatenate(xs, ys), 4)


let langStar (xs,n) =
  restrict(all_strings(xs, n), n)



(* QUESTION 3 *)


(* some helper code -- vaguely advanced OCaml in here, but barely *)

type re = Empty | Unit | Letter of string | Plus of re * re | Times of re * re | Star of re

let lang (s,n) =
  let fromChar c = String.make 1 c in
  let explode s =
    let rec loop i result =
      if i < 0 then result
      else loop (i-1) (s.[i]::result) in
    loop (String.length s - 1) []  in
  (* Grammar:
   *
   * R ::= R1 + R
   *       R1
   *
   * R1 ::= R2 R1
   *        R2
   *
   * R2 ::= R3*
   *        R3
   *
   * R3 ::= a
   *        1
   *        0
   *        ( R )
   *)
  let isalpha = function 'A'..'Z'|'a'..'z' -> true | _ -> false in
  let expect c cs =
    match cs with
      f::cs when f = c -> Some cs
    | _ -> None in
  let expect_alpha cs =
    match cs with
      f::cs when isalpha f -> Some (f,cs)
    | _ -> None  in
  let rec parse_R cs =
    match parse_R1 cs with
      None -> None
    | Some (r1,cs) ->
        (match expect '+' cs with
           None -> Some (r1,cs)
         | Some cs ->
             (match parse_R cs with
                None -> None
              | Some (r2,cs) -> Some (Plus(r1,r2),cs)))
  and parse_R1 cs =
    match parse_R2 cs with
      None -> None
    | Some (r1,cs) ->
        (match parse_R1 cs with
           None -> Some (r1,cs)
         | Some (r2,cs) -> Some (Times(r1,r2),cs))
  and parse_R2 cs =
    match parse_R3 cs with
      None -> None
    | Some (r1,cs) ->
        (match expect '*' cs with
           None -> Some (r1,cs)
         | Some cs -> Some (Star(r1),cs))
  and parse_R3 cs =
    match expect_alpha cs with
      Some (a,cs) -> Some (Letter(fromChar(a)),cs)
    | None ->
        (match expect '1' cs with
           Some cs -> Some (Unit, cs)
         | None ->
             (match expect '0' cs with
                Some cs -> Some (Empty,cs)
              | None -> parse_parens cs))
  and parse_parens cs =
    match expect '(' cs with
      None -> None
    | Some cs ->
        (match parse_R cs with
           None -> None
         | Some (r,cs) ->
             (match expect ')' cs with
                None -> None
              | Some cs -> Some (r,cs)))  in
  let parse s =
    let cs = explode s in
    match parse_R cs with
      Some (re,[]) -> re
    | _ -> failwith ("Cannot parse "^s)  in
  let rec eval re =
    match re with
      Empty -> []
    | Unit -> [""]
    | Letter (a) -> [a]
    | Plus (r1,r2) -> langUnion(eval r1,eval r2,n)
    | Times (r1,r2) -> langConcat(eval r1,eval r2,n)
    | Star r -> langStar(eval r,n)  in
    eval (parse s)

let dump l =
  List.iter (fun s -> match s with "" -> print_string "  <empty>\n"
                                 | s -> print_string ("  "^s^"\n")) l



(* Placeholder for your regular expression. Replace "0" by your actual answer *)

let regexp_a = "0"

let regexp_b = "0"

let regexp_c = "0"

let regexp_d = "0"

let regexp_e = "0"
