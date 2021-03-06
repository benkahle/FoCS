(*

HOMEWORK 6

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
 *
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
 *   Code to run a string tm machine
 *
 *)

let run m w =

  let printConfig m config value =
    let mw =
      List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ =
      print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	       print_syms v') in
    let _ = print_newline ()  in
    value  in

  let acceptConfig m config = (config.state=m.accept) in

  let rejectConfig m config = (config.state=m.reject) in

  let haltConfig m c = (acceptConfig m c) || (rejectConfig m c) in

  let startConfig m w =
    { state=m.start;before = [];after = m.left_marker::(explode w)} in

  let rec last u =
    match u with
    | [] -> failwith "Moving Left from leftmost tape position"
    | [a] -> ([],a)
    | x::xs -> let (u',r) = last xs  in (x::u',r)   in

  let step m config =
    if (haltConfig m config) then config
    else let (a,v') = match config.after with
                      | [] -> (m.blank,[])
		      | a::v' -> (a,v')  in
         let (q',b,dir) = m.delta(config.state,a) in
	 if dir = 0  (* left *)
	 then let (u',c) = last config.before in
  	      {state=q';before=u';after=c::b::v'}
	 else {state=q';before=config.before@[b];after=v'} in

  let rec loop c =
    let _ = printConfig m c c in
    if  (acceptConfig m c) then true
    else if (rejectConfig m c) then false
    else loop (step m c)  in

  loop (startConfig m w)


let rec pairs xs ys =
  List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []

(* QUESTION 1 *)

let triples xs ys zs =
  let ps = pairs ys zs in
  List.fold_right (fun x r -> (List.map (fun (a, b) -> (x, a, b)) ps)@r) xs []

let quads xs ys zs ws =
  let ts = triples ys zs ws in
  List.fold_right (fun x r -> (List.map (fun (a, b, c) -> (x, a, b, c)) ts)@r) xs []

let rec range n =
  if n < 0 then []
  else if n = 0 then [0]
  else n :: range (n-1)

(* QUESTION 2 *)

let transformStates states f =
  List.map (fun state -> f state) states

let find_original states f target =
  try
    List.find (fun state -> if (f state) = target then true else false) states
  with
    Not_found -> failwith "state not found"

let transformDelta states delta f =
  (fun (p, a) ->
    let (tr, x, y) = delta ((find_original states f p), a) in
    (f tr, x, y)
  )

let transform m f =
  {
    states = transformStates m.states f;
    input_alphabet = m.input_alphabet;
    tape_alphabet = m.tape_alphabet;
    blank = m.blank;
    left_marker = m.left_marker;
    start = f m.start;
    accept = f m.accept;
    reject = f m.reject;
    delta = transformDelta m.states m.delta f
  }




(*
 * Some sample deterministic Turing machines with structured states
 *
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * add1  accepts strings u#v where v = u+1 in binary
 *
 *)


let anbn = { states = [ ("start",0);
			("q",1);
			("q",2);
			("q",3);
			("q",4);
			("acc",0);
			("rej",0) ];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = ("start",0);
	     accept = ("acc",0);
	     reject = ("rej",0);
	     delta = (fun inp -> match inp with
	                 | (("start",0), "a") -> (("start",0), "a", 1)
     			 | (("start",0), "b") -> (("q",1), "b", 1)
			 | (("start",0), "|") -> (("start",0), "|", 1)
			 | (("start",0), "/") -> (("q",2), "/", 1)
			 | (("q",1), "b") -> (("q",1), "b", 1)
			 | (("q",1), "/") -> (("q",2), "/", 1)
			 | (("q",2), "|") -> (("q",3), "|", 1)
			 | (("q",2), "a") -> (("q",2), "a", 0)
			 | (("q",2), "b") -> (("q",2), "b", 0)
			 | (("q",2), "X") -> (("q",2), "X", 0)
			 | (("q",2), "/") -> (("q",2), "/", 0)
			 | (("q",3), "X") -> (("q",3), "X", 1)
			 | (("q",3), "/") -> (("acc",0), "/", 1)
			 | (("q",3), "a") -> (("q",4), "X", 1)
			 | (("q",4), "a") -> (("q",4), "a", 1)
			 | (("q",4), "X") -> (("q",4), "X", 1)
			 | (("q",4), "b") -> (("q",2), "X", 1)
			 | (("acc",0), s) -> (("acc",0),s,1)
			 | (_,c) -> (("rej",0),c,1))}


let add1 =
  { states =    (* spelled out fully so as not to rely on 'triples' *)
[("start", -1, -1); ("start", -1, 0); ("start", -1, 1); ("start", 0, -1);
 ("start", 0, 0); ("start", 0, 1); ("start", 1, -1); ("start", 1, 0);
 ("start", 1, 1); ("check1", -1, -1); ("check1", -1, 0); ("check1", -1, 1);
 ("check1", 0, -1); ("check1", 0, 0); ("check1", 0, 1); ("check1", 1, -1);
 ("check1", 1, 0); ("check1", 1, 1); ("check2", -1, -1); ("check2", -1, 0);
 ("check2", -1, 1); ("check2", 0, -1); ("check2", 0, 0); ("check2", 0, 1);
 ("check2", 1, -1); ("check2", 1, 0); ("check2", 1, 1); ("rewind", -1, -1);
 ("rewind", -1, 0); ("rewind", -1, 1); ("rewind", 0, -1); ("rewind", 0, 0);
 ("rewind", 0, 1); ("rewind", 1, -1); ("rewind", 1, 0); ("rewind", 1, 1);
 ("go-end-1", -1, -1); ("go-end-1", -1, 0); ("go-end-1", -1, 1);
 ("go-end-1", 0, -1); ("go-end-1", 0, 0); ("go-end-1", 0, 1);
 ("go-end-1", 1, -1); ("go-end-1", 1, 0); ("go-end-1", 1, 1);
 ("go-end-2", -1, -1); ("go-end-2", -1, 0); ("go-end-2", -1, 1);
 ("go-end-2", 0, -1); ("go-end-2", 0, 0); ("go-end-2", 0, 1);
 ("go-end-2", 1, -1); ("go-end-2", 1, 0); ("go-end-2", 1, 1);
 ("skip", -1, -1); ("skip", -1, 0); ("skip", -1, 1); ("skip", 0, -1);
 ("skip", 0, 0); ("skip", 0, 1); ("skip", 1, -1); ("skip", 1, 0);
 ("skip", 1, 1); ("scan-1", -1, -1); ("scan-1", -1, 0); ("scan-1", -1, 1);
 ("scan-1", 0, -1); ("scan-1", 0, 0); ("scan-1", 0, 1); ("scan-1", 1, -1);
 ("scan-1", 1, 0); ("scan-1", 1, 1); ("scan-2", -1, -1); ("scan-2", -1, 0);
 ("scan-2", -1, 1); ("scan-2", 0, -1); ("scan-2", 0, 0); ("scan-2", 0, 1);
 ("scan-2", 1, -1); ("scan-2", 1, 0); ("scan-2", 1, 1);
 ("check-done", -1, -1); ("check-done", -1, 0); ("check-done", -1, 1);
 ("check-done", 0, -1); ("check-done", 0, 0); ("check-done", 0, 1);
 ("check-done", 1, -1); ("check-done", 1, 0); ("check-done", 1, 1)];
    input_alphabet = ["0";"1";"#"];
    tape_alphabet = ["0";"1";"#";"X";"_";">"];
    blank = "_";
    left_marker = ">";
    start = ("start",-1,-1);
    accept = ("acc",-1,-1);
    reject = ("rej",-1,-1);
    delta = (fun x -> match x with
    | (("start",-1,-1),">") -> (("check1",-1,-1),">",1)
    | (("check1",-1,-1),"0") -> (("check1",-1,-1),"0",1)
    | (("check1",-1,-1),"1") -> (("check1",-1,-1),"1",1)
    | (("check1",-1,-1),"#") -> (("check2",-1,-1),"#",1)
    | (("check2",-1,-1),"0") -> (("check2",-1,-1),"0",1)
    | (("check2",-1,-1),"1") -> (("check2",-1,-1),"1",1)
    | (("check2",-1,-1),"_") -> (("rewind",-1,1),"_",0)   (* start with a carry of 1! *)

    | (("rewind",-1,carry),">") -> (("go-end-1",-1,carry),">",1)
    | (("rewind",-1,carry),"0") -> (("rewind",-1,carry),"0",0)
    | (("rewind",-1,carry),"1") -> (("rewind",-1,carry),"1",0)
    | (("rewind",-1,carry),"#") -> (("rewind",-1,carry),"#",0)
    | (("rewind",-1,carry),"X") -> (("rewind",-1,carry),"X",0)

    | (("go-end-1",-1,carry),"#") -> (("scan-1",-1,carry),"#",0)
    | (("go-end-1",-1,carry),sym) -> (("go-end-1",-1,carry),sym,1)

    | (("scan-1",-1,carry),"X") -> (("scan-1",-1,carry),"X",0)
    | (("scan-1",-1,carry),"0") -> (("skip",0,carry),"X",1)
    | (("scan-1",-1,carry),"1") -> (("skip",1,carry),"X",1)
    | (("scan-1",-1,0),">") -> (("check-done",-1,-1),">",1)  (* carry should be 0 to be done *)

    | (("skip",v,carry),"#") -> (("go-end-2",v,carry),"#",1)
    | (("skip",v,carry),"X") -> (("skip",v,carry),"X",1)

    | (("go-end-2",v,carry),"_") -> (("scan-2",v,carry),"_",0)
    | (("go-end-2",v,carry),sym) -> (("go-end-2",v,carry),sym,1)

    | (("scan-2",v,carry),"X") -> (("scan-2",v,carry),"X",0)
    | (("scan-2",v,carry),"0") when (v+carry) mod 2 = 0 -> (("rewind",-1,(v+carry) / 2),"X",0)
    | (("scan-2",v,carry),"1") when (v+carry) mod 2 = 1 -> (("rewind",-1,(v+carry) / 2),"X",0)

    | (("check-done",-1,-1),"_") -> (("acc",-1,-1),"_",1)
    | (("check-done",-1,-1),"X") -> (("check-done",-1,-1),"X",1)
    | (("check-done",-1,-1),"#") -> (("check-done",-1,-1),"#",1)

    | (_,sym) -> (("rej",-1,-1),sym,1))}





(* QUESTION 3 *)

let structPerms =
  { states = [("start", "x"); ("find", "x"); ("rw", "x"); ("end", "x"); ("acc", "x"); ("rej", "x")] @ pairs ["next";"match"] ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"x";"y";"z"];
    input_alphabet = ["#";"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"x";"y";"z"];
    tape_alphabet = ["X";">";"_";"#";"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"x";"y";"z"];
    start = ("start", "x");
    accept = ("acc", "x");
    reject = ("rej", "x");
    blank = "_";
    left_marker = ">";
    delta = (fun x -> match x with
      | (("start", "x"), ">") -> (("find", "x"), ">", 1)

      | (("find", "x"), "X") -> (("find", "x"), "X", 1)
      | (("find", "x"), "#") -> (("end", "x"), "#", 1)
      | (("find", "x"), sym) -> (("next", sym), "X", 1) (*Reject?*)

      | (("end", "x"), "X") -> (("end", "x"), "X", 1)
      | (("end", "x"), "_") -> (("acc", "x"), "_", 1)

      | (("next", sym), "_") -> (("rej", "x"), "_", 1)
      | (("next", sym), "#") -> (("match", sym), "#", 1)
      | (("next", sym), any) -> (("next", sym), any, 1)

      | (("match", sym), "_") -> (("rej", "x"), "_", 1)
      | (("match", sym), ch) when (sym = ch) -> (("rw", "x"), "X", 0)
      | (("match", sym), any) -> (("match", sym), any, 1)

      | (("rw", "x"), ">") -> (("find", "x"), ">", 1)
      | (("rw", "x"), any) -> (("rw", "x"), any, 0)

      | ((_,sym), any) -> (("rej", "x"),any,1)
    )
  }

let pairStringTransform (x, y) = x^"/"^y

let permutation = transform structPerms pairStringTransform


let copies n =
  if (n <= 0) then failwith "Length must be greater than 0" else
  let structCopies =
    {
      states = [("start", -1, "-1");("acc", -1, "-1");("rej", -1, "-1");("rw", -1, "-1");] @ triples ["find"; "next"] (range n) ["0";"1";"#";"_"];
      input_alphabet = ["#";"0";"1"];
      tape_alphabet = ["#";"0";"1";"_";">";"X"];
      start = ("start", -1, "-1");
      accept = ("acc", -1, "-1");
      reject = ("rej", -1, "-1");
      blank = "_";
      left_marker = ">";
      delta = (fun x -> match x with
        | (("start", -1, "-1"), ">") -> (("find", n, "#"), ">", 1)

        | (("find", 0, "#"), "#") -> (("rej", -1, "-1"), "#", 1) (*You've gone too far!*)
        | (("find", h, m), "X") -> (("find", h, m), "X", 1)
        | (("find", h, m), "#") -> (("find", h-1, "#"), "#", 1)
        | (("find", 1, "#"), "_") -> (("acc", -1, "-1"), "_", 1)
        | (("find", h, m), any) when (h = n) -> (("next", h, any), "X", 1)
        | (("find", h, m), a) when (m = a) -> (("next", h, a), "X", 1)

        | (("next", 1, m), "_") when (m = "#" || m = "_") -> (("acc", -1, "-1"), "_", 1)
        | (("next", 1, m), "#") -> (("rej", -1, "-1"), "#", 1)
        | (("next", 1, m), any) -> (("rw", -1, "-1"), any, 0)
        | (("next", h, m), "#") -> (("find", h-1, m), "#", 1)
        | (("next", h, m), "_") -> (("rej", -1, "-1"), "_", 1)
        | (("next", h, m), any) -> (("next", h, m), any, 1)

        | (("rw", -1, "-1"), ">") -> (("find", n, "#"), ">", 1)
        | (("rw", -1, "-1"), any) -> (("rw", -1, "-1"), any, 0)

        | ((_, h, mem), any) -> (("rej", -1, "-1"), any, 1)
      )
    }
  in
  let copiesTransform (x, y, z) = x^"/"^(string_of_int y)^"/"^z in
  transform structCopies copiesTransform
