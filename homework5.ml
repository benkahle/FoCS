(*

HOMEWORK 5

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




(*
 * String <-> characters utility functions:
 *
 *   explode : string -> string list
 *      returns the list of characters making up a string
 *
 *)

let explode str =
  let rec acc index result =
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type symbol = string

type 'a tm = { states : 'a list;
	       input_alphabet : symbol list;
	       tape_alphabet : symbol list;
	       left_marker : symbol;
	       blank : symbol;
	       delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
	       start : 'a;
	       accept : 'a;
	       reject : 'a }

type 'a config = { state : 'a;
		   before: symbol list;
		   after: symbol list }

(*
 * Helper function
 *
 * Print a configuration (including newline) to standard output
 * and RETURN A VALUE
 *
 *)

let printConfig m config value =
    let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	                print_syms v') in
    let _ = print_newline ()  in
    value




(* QUESTION 1 *)


let startConfig m w =
  {
    state = m.start;
    before = [];
    after = m.left_marker :: (explode w)
  }

let acceptConfig m config =
  config.state = m.accept

let rejectConfig m config =
  config.state = m.reject

let haltConfig m c =
  acceptConfig m c || rejectConfig m c

let step m config =
  let (q, b, dir) = m.delta (config.state, (match config.after with [] -> m.blank | x::xs -> x)) in
  {
    state = q;
    before = if dir = 1 then
      config.before @ [b]
    else
      List.rev (List.tl (List.rev config.before));
    after = if dir = 1 then
      match config.after with [] -> [] | x::xs -> xs
    else
      (List.nth config.before (List.length config.before - 1))::(b::(match config.after with [] -> [] | x::xs -> xs));
  }

let run m w =
  let rec iter m config =
    if haltConfig m config then config else iter m (printConfig m config (step m config))
  in
  let finalCfg = iter m (startConfig m w) in
  printConfig m finalCfg (acceptConfig m finalCfg)



(*
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"_";">"];
	     blank = "_";
	     left_marker = ">";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", ">") -> ("start", ">", 1)
			 | ("start", "_") -> ("acc", "_", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "_") -> ("acc", "_", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", ">") -> ("acc", ">", 1)
			 | ("acc", "_") -> ("acc", "_", 1)
			 | (_,c) -> ("rej",c,1))}

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", "|") -> ("start", "|", 1)
			 | ("start", "/") -> ("q2", "/", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "/") -> ("q2", "/", 1)
			 | ("q2", "|") -> ("q3", "|", 1)
			 | ("q2", "a") -> ("q2", "a", 0)
			 | ("q2", "b") -> ("q2", "b", 0)
			 | ("q2", "X") -> ("q2", "X", 0)
			 | ("q2", "/") -> ("q2", "/", 0)
			 | ("q3", "X") -> ("q3", "X", 1)
			 | ("q3", "/") -> ("acc", "/", 1)
			 | ("q3", "a") -> ("q4", "X", 1)
			 | ("q4", "a") -> ("q4", "a", 1)
			 | ("q4", "X") -> ("q4", "X", 1)
			 | ("q4", "b") -> ("q2", "X", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", "|") -> ("acc", "|", 1)
			 | ("acc", "X") -> ("acc", "X", 1)
			 | ("acc", "/") -> ("acc", "/", 1)
			 | (_,c) -> ("rej",c,1))}


let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       input_alphabet = ["a";"b";"c"];
	       tape_alphabet = ["a";"b";"c";"X";"_";">"];
	       blank = "_";
	       left_marker = ">";
	       start = "start";
	       accept = "acc";
	       reject = "rej";
	       delta = (fun inp -> match inp with
	                | ("start", "a") -> ("start", "a", 1)
     			| ("start", "b") -> ("q1", "b", 1)
			| ("start", "c") -> ("q6", "c", 1)
			| ("start", ">") -> ("start", ">", 1)
			| ("start", "_") -> ("q2", "_", 1)
			| ("q1", "b") -> ("q1", "b", 1)
			| ("q1", "c") -> ("q6", "c", 1)
			| ("q1", "_") -> ("q2", "_", 1)
			| ("q2", ">") -> ("q3", ">", 1)
			| ("q2", "a") -> ("q2", "a", 0)
			| ("q2", "b") -> ("q2", "b", 0)
			| ("q2", "c") -> ("q2", "c", 0)
			| ("q2", "_") -> ("q2", "_", 0)
			| ("q2", "X") -> ("q2", "X", 0)
			| ("q3", "X") -> ("q3", "X", 1)
			| ("q3", "_") -> ("acc", "_", 1)
			| ("q3", "a") -> ("q4", "X", 1)
			| ("q4", "a") -> ("q4", "a", 1)
			| ("q4", "X") -> ("q4", "X", 1)
			| ("q4", "b") -> ("q5", "X", 1)
			| ("q5", "b") -> ("q5", "b", 1)
			| ("q5", "X") -> ("q5", "X", 1)
			| ("q5", "c") -> ("q2", "X", 1)
			| ("q6", "c") -> ("q6", "c", 1)
			| ("q6", "_") -> ("q2", "_", 1)
		        | ("acc", "a") -> ("acc", "a", 1)
		        | ("acc", "b") -> ("acc", "b", 1)
		        | ("acc", "c") -> ("acc", "c", 1)
		        | ("acc", ">") -> ("acc", ">", 1)
		        | ("acc", "X") -> ("acc", "X", 1)
		        | ("acc", "_") -> ("acc", "_", 1)
			| (_,c) -> ("rej", c,1))}



(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY TURING MACHINES *)
(* REPLACE BY YOUR OWN DEFINITIONS *)


let tm_q2_a = { states = ["start"; "C"; "D"; "C?"; "D?"; "RW"; "acc"; "rej"];
		input_alphabet = ["c"; "d"];
		tape_alphabet = ["c"; "d"; "Z"; "X"; "_"; ">"];
		blank = "_";
		left_marker = ">";
		start = "start";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with
      | ("start", ">") -> ("start", ">", 1)
      | ("start", "X") -> ("acc", "X", 1)
      | ("start", "_") -> ("acc", "_", 1)
      | ("start", "c") -> ("C", "Z", 1)
      | ("start", "d") -> ("D", "Z", 1)
      | ("C", "c") -> ("C", "c", 1)
      | ("C", "d") -> ("C", "d", 1)
      | ("C", "_") -> ("C?", "_", 0)
      | ("C", "X") -> ("C?", "X", 0)
      | ("C?", "Z") -> ("acc", "Z", 1)
      | ("C?", "d") -> ("rej", "d", 1)
      | ("C?", "c") -> ("RW", "X", 0)
      | ("RW", "c") -> ("RW", "c", 0)
      | ("RW", "d") -> ("RW", "d", 0)
      | ("RW", "Z") -> ("start", "Z", 1)
      | ("D", "c") -> ("D", "c", 1)
      | ("D", "d") -> ("D", "d", 1)
      | ("D", "_") -> ("D?", "_", 0)
      | ("D", "X") -> ("D?", "X", 0)
      | ("D?", "Z") -> ("acc", "Z", 1)
      | ("D?", "c") -> ("rej", "c", 1)
      | ("D?", "d") -> ("RW", "X", 0)
      | (_, c) -> ("rej", c, 1)
    )
  }


let tm_q2_b = { states = ["start"; "V"; "RW"; "B1"; "BX"; "AX"; "E"; "A-1"; "A-2"; "acc"; "rej"];
		input_alphabet = ["a"; "b"];
		tape_alphabet = ["a"; "b"; "X"; "Z"; "_"; ">"];
		blank = "_";
		left_marker = ">";
		start = "start";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with
      | ("start", ">") -> ("start", ">", 1)
      | ("start", "b") -> ("start", "b", 1)
      | ("start", "a") -> ("V", "a", 1)
      | ("start", "_") -> ("RW", "_", 0)
      | ("V", "a") -> ("V", "a", 1)
      | ("V", "_") -> ("RW", "_", 0)
      | ("RW", "a") -> ("RW", "a", 0)
      | ("RW", "b") -> ("RW", "b", 0)
      | ("RW", "X") -> ("B1", "X", 1)
      | ("RW", ">") -> ("B1", ">", 1)
      | ("B1", "Z") -> ("acc", "Z", 1)
      | ("B1", "_") -> ("acc", "_", 1)
      | ("B1", "b") -> ("BX", "X", 1)
      | ("BX", "b") -> ("BX", "b", 1)
      | ("BX", "a") -> ("AX", "a", 1)
      | ("AX", "a") -> ("AX", "a", 1)
      | ("AX", "Z") -> ("E", "Z", 0)
      | ("AX", "_") -> ("E", "_", 0)
      | ("E", "a") -> ("A-1", "Z", 0)
      | ("A-1", "a") -> ("A-2", "Z", 0)
      | ("A-2", "a") -> ("RW", "Z", 0)
      | (_, c) -> ("rej", c, 1)
      )
    }




(* QUESTION 3 *)


let binaryAddition = { states = ["x"];
		       input_alphabet = ["x"];
		       tape_alphabet = ["x"];
		       blank = "x";
		       left_marker = "x";
		       start = "x";
		       accept = "x";
		       reject = "x";
		       delta = (fun (x,y) -> (x,y,0))}
