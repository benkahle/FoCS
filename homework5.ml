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


let binaryAddition = { states = ["start";"X1";"X2";"#1";"#2";"E";"D?";"O";"Z";"Z1";"ZNC";"ZNCD";"ZC";"ZCD";"FF";"RW";"FFC";"RWC";"O1";"OZ";"OZD";"OO";"OOD";"acc";"rej"];
		       input_alphabet = ["1";"0";"#"];
		       tape_alphabet = ["1";"0";"#";"X";">";"_"];
		       blank = "_";
		       left_marker = ">";
		       start = "start";
		       accept = "acc";
		       reject = "rej";
		       delta = (fun input -> match input with
             | ("start", ">") -> ("start", ">", 1)
             | ("start", "1") -> ("X1", "1", 1)
             | ("start", "0") -> ("X1", "0", 1)
             | ("X1", "#") -> ("#1", "#", 1)
             | ("X1", "1") -> ("X1", "1", 1)
             | ("X1", "0") -> ("X1", "0", 1)
             | ("#1", "1") -> ("X2", "1", 1)
             | ("#1", "0") -> ("X2", "0", 1)
             | ("X2", "#") -> ("#2", "#", 1)
             | ("X2", "1") -> ("X2", "1", 1)
             | ("X2", "0") -> ("X2", "0", 1)
             | ("#2", "1") -> ("#2", "1", 1)
             | ("#2", "0") -> ("#2", "0", 1)
             | ("#2", "_") -> ("E", "_", 0)
             | ("E", "#") -> ("D?", "#", 0)
             | ("E", "0") -> ("Z", "X", 0)
             | ("E", "1") -> ("O", "X", 0)
             | ("D?", "X") -> ("D?", "X", 0)
             | ("D?", "#") -> ("D?", "#", 0)
             | ("D?", ">") -> ("acc", ">", 1)
             | ("Z", "0") -> ("Z", "0", 0)
             | ("Z", "1") -> ("Z", "1", 0)
             | ("Z", "#") -> ("Z1", "#", 0)
             | ("Z1", "X") -> ("Z1", "X", 0)
             | ("Z1", "0") -> ("ZNC", "X", 0)
             | ("Z1", "#") -> ("ZNCD", "#", 0)
             | ("Z1", "1") -> ("ZC", "X", 0)
             | ("ZNC", "0") -> ("ZNC", "0", 0)
             | ("ZNC", "1") -> ("ZNC", "1", 0)
             | ("ZNC", "#") -> ("ZNCD", "#", 0)
             | ("ZNCD", "X") -> ("ZNCD", "X", 0)
             | ("ZNCD", "0") -> ("FF", "X", 1)
             | ("ZNCD", ">") -> ("FF", ">", 1)
             | ("ZC", "0") -> ("ZC", "0", 0)
             | ("ZC", "1") -> ("ZC", "1", 0)
             | ("ZC", "#") -> ("ZCD", "#", 0)
             | ("ZCD", "X") -> ("ZCD", "X", 0)
             | ("ZCD", "1") -> ("FFC", "X", 1)
             | ("FF", "#") -> ("FF", "#", 1)
             | ("FF", "0") -> ("FF", "0", 1)
             | ("FF", "1") -> ("FF", "1", 1)
             | ("FF", "X") -> ("FF", "X", 1)
             | ("FF", "_") -> ("RW", "_", 0)
             | ("RW", "X") -> ("RW", "X", 0)
             | ("RW", "#") -> ("D?", "#", 0)
             | ("RW", "0") -> ("Z", "X", 0)
             | ("RW", "1") -> ("O", "X", 0)
             | ("FFC", "#") -> ("FFC", "#", 1)
             | ("FFC", "0") -> ("FFC", "0", 1)
             | ("FFC", "1") -> ("FFC", "1", 1)
             | ("FFC", "X") -> ("FFC", "X", 1)
             | ("FFC", "_") -> ("RWC", "_", 0)
             | ("RWC", "X") -> ("RWC", "X", 0)
             | ("RWC", "1") -> ("Z", "X", 0)
             | ("RWC", "0") -> ("OC", "X", 0)
             | ("O", "0") -> ("O", "0", 0)
             | ("O", "1") -> ("O", "1", 0)
             | ("O", "#") -> ("O1", "#", 0)
             | ("O1", "X") -> ("O1", "X", 0)
             | ("O1", "0") -> ("OZ", "X", 0)
             | ("O1", "#") -> ("OZD", "#", 0)
             | ("O1", "1") -> ("OO", "X", 0)
             | ("OZ", "0") -> ("OZ", "0", 0)
             | ("OZ", "1") -> ("OZ", "1", 0)
             | ("OZ", "#") -> ("OZD", "#", 0)
             | ("OZD", "X") -> ("OZD", "X", 0)
             | ("OZD", "1") -> ("FF", "X", 1)
             | ("OO", "0") -> ("OO", "0", 0)
             | ("OO", "1") -> ("OO", "1", 0)
             | ("OO", "#") -> ("OOD", "#", 0)
             | ("OOD", "X") -> ("OOD", "X", 0)
             | ("OOD", "0") -> ("FF", "X", 1)
             | ("OOD", ">") -> ("FF", ">", 1)
             | ("OC", "0") -> ("OC", "0", 0)
             | ("OC", "1") -> ("OC", "1", 0)
             | ("OC", "#") -> ("O1C", "#", 0)
             | ("O1C", "X") -> ("O1C", "X", 0)
             | ("O1C", "0") -> ("OZC", "X", 0)
             | ("O1C", "#") -> ("OZDC", "#", 0)
             | ("O1C", "1") -> ("OOC", "X", 0)
             | ("OZC", "0") -> ("OZC", "0", 0)
             | ("OZC", "1") -> ("OZC", "1", 0)
             | ("OZC", "#") -> ("OZDC", "#", 0)
             | ("OZDC", "X") -> ("OZDC", "X", 0)
             | ("OZDC", "1") -> ("FFC", "X", 1)
             | ("OOC", "0") -> ("OOC", "0", 0)
             | ("OOC", "1") -> ("OOC", "1", 0)
             | ("OOC", "#") -> ("OODC", "#", 0)
             | ("OODC", "X") -> ("OODC", "X", 0)
             | ("OODC", "0") -> ("FFC", "X", 1)
             | ("OODC", ">") -> ("FFC", ">", 1)
             | (_, c) -> ("rej", c, 1)
             )
           }
