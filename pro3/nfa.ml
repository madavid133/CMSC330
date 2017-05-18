(* CMSC 330 Organization of Programming Languages 
_____Fall_2016_____ FA2016P3REGEXOCAML
Dr. Anwar Mamat 
Project 3: Regular Expression Interpreter
 
*)

#load "str.cma"

(* ------------------------------------------------- *)
(* MODULE SIGNATURE *)
(* ------------------------------------------------- *)

module type NFA =
  sig
    (* You may NOT change this signature *)

    (* ------------------------------------------------- *)
    (* PART 1: NFA IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Abstract type for NFAs *)
    type nfa

    (* Type of an NFA transition.

       (s0, Some c, s1) represents a transition from state s0 to state s1
       on character c

       (s0, None, s1) represents an epsilon transition from s0 to s1
     *)
    type transition = int * char option * int

    (* ------------------------------------------------- *)
    (* Returns a new NFA.  make_nfa s fs ts returns an NFA with start
       state s, final states fs, and transitions ts.
     *)
    val make_nfa : int -> int list -> transition list -> nfa

    (* ------------------------------------------------- *)
    (*  Calculates epsilon closure in an NFA.

	e_closure m ss returns a list of states that m could
	be in, starting from any state in ss and making 0 or
	more epsilon transitions.

       There should be no duplicates in the output list of states.
     *)

    val e_closure : nfa -> int list -> int list

    (* ------------------------------------------------- *)
    (*  Calculates move in an NFA.

	move m ss c returns a list of states that m could
	be in, starting from any state in ss and making 1
	transition on c.

       There should be no duplicates in the output list of states.
     *)

    val move : nfa -> int list -> char -> int list

    (* ------------------------------------------------- *)
    (* Returns true if the NFA accepts the string, and false otherwise *)
    val accept : nfa -> string -> bool

    (* ------------------------------------------------- *)
    (* Gives the stats of the NFA

      the first integer representing the number of states
      the second integer representing the number of final states
      the (int * int) list represents the number of states with a particular number of transitions
      e.g. (0,1) means there is 1 state with 0 transitions, (1,2) means there is 2 states with 1 transition
      the list would look something like: [(0,1);(1,2);(2,3);(3,1)]

    *)

    val stats : nfa -> int * int * (int * int) list

    (* ------------------------------------------------- *)
    (* PART 2: REGULAR EXPRESSION IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    type regexp =
	Empty_String
      | Char of char
      | Union of regexp * regexp
      | Concat of regexp * regexp
      | Star of regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression, print it as a regular expression in
       postfix notation (as in project 2).  Always print the first regexp
       operand first, so output string will always be same for each regexp.
     *)
    val regexp_to_string : regexp -> string

    (* ------------------------------------------------- *)
    (* Given a regular expression, return an nfa that accepts the same
       language as the regexp
     *)
    val regexp_to_nfa : regexp -> nfa

    (* ------------------------------------------------- *)
    (* PART 3: REGULAR EXPRESSION PARSER *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns the
       equivalent regular expression represented as the type regexp.
     *)
    val string_to_regexp : string -> regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns
       the equivalent nfa
     *)
    val string_to_nfa: string -> nfa

    (* ------------------------------------------------- *)
    (* Throw IllegalExpression expression when regular
       expression syntax is illegal
     *)
    exception IllegalExpression of string

end

(* ------------------------------------------------- *)
(* MODULE IMPLEMENTATION *)
(* ------------------------------------------------- *)

    (* Make all your code changes past this point *)
    (* You may add/delete/reorder code as you wish
       (but note that it still must match the signature above) *)

module NfaImpl =
struct

type transition = int * char option * int

type nfa = {ss: int; fs: int list; ts:  transition list} 

let make_nfa ss fs ts = {ss = ss; fs = fs; ts = ts};;

let addOne m states state = 
    let valid_transition = List.filter (fun t -> match t with
       (s, None, _) when s = state -> true
       | _ -> false) m.ts in
    let added = List.map (fun (_, _, des) -> des) valid_transition in
    let ns = List.append states added in 
    List.sort_uniq (fun a b -> a - b) ns
;;

 let rec e_closure_helper m cur =
    let init = cur in 
    let ns = List.fold_left (addOne m) init cur in
    if(ns = cur) then cur else e_closure_helper m ns
;;

let e_closure m ss = 
  e_closure_helper m (List.sort_uniq (fun a b -> a - b) ss)
;;

let moveOne m c states state  =
  let valid_transition = List.filter (fun t -> match t with
      (s, Some u, _) when s = state && u = c -> true
      | _ -> false) m.ts in
    let added = List.map (fun (_, _, des) -> des) valid_transition in
    let ns = List.append states added in 
    List.sort_uniq (fun a b -> a - b) ns
;;
  
let move m ss c = 
  List.fold_left (moveOne m c) [] (List.sort_uniq (fun a b -> a - b) ss)
;;

let rec acceptH m s states =
  let closure = e_closure m states in 
    if (String.length s) = 0 
    then List.exists (fun a -> List.mem a m.fs) closure
    else acceptH m (String.sub s 1 ((String.length s) - 1) ) (move m closure (String.get s 0))
;;

let accept m s = acceptH m s [m.ss]
;;

let getAllStates m = 
  let allstates = List.fold_left (fun l (s, _, e) -> s::e::l) (m.ss::m.fs) m.ts in 
  List.sort_uniq (fun a b -> a - b) allstates
;;

let getAllOutNumber m states = 
    List.map (fun s -> 
      let gooddTs = List.filter (fun t -> match t with
      (x, _, _) when x = s -> true
      | _ -> false) m.ts in
      List.length gooddTs) states
;;

let computeOut numOut = 
  List.fold_left (fun ass num ->
    if List.mem_assoc num ass 
    then (num, (List.assoc num ass) + 1) :: (List.remove_assoc num ass)
    else (num, 1):: ass)  
    [] numOut
;;


let stats m = 
  let allStates = getAllStates m in 
  let numOut = getAllOutNumber m allStates in 
  let l = List.sort (fun (a, _) (b, _) -> a - b) (computeOut numOut) in 
  (List.length allStates, List.length m.fs, l)
;;

let next = 
  let count = ref 0 in 
    function () ->
      let temp = !count in 
        count := (!count) + 1;
        temp
;;


type regexp =
	  Empty_String
	| Char of char
	| Union of regexp * regexp
	| Concat of regexp * regexp
	| Star of regexp


let rec regexp_to_list_string r = match r with
   Empty_String -> ["E"]
   | Char(x) -> [String.make 1 x]
   | Union(a, b) -> (regexp_to_list_string a) @ (regexp_to_list_string b) @ ["|"]
   | Concat(a, b) -> (regexp_to_list_string a) @ (regexp_to_list_string b) @ ["."]
   | Star(a) -> (regexp_to_list_string a) @ ["*"]

let  regexp_to_string r =
    let l = regexp_to_list_string r in 
    String.concat " " l
;;


let rec regexp_to_nfa r = match r with
   Empty_String -> let q = next() in
           let f = next() in 
           make_nfa q [f] [(q, None, f)]
   | Char(x) -> let q = next() in
           let f = next() in 
           make_nfa q [f] [(q, Some x, f)]
   | Union(a, b) -> 

           let q = next() in
           let f = next() in 
           let x = regexp_to_nfa a in
           let y = regexp_to_nfa b in

           make_nfa q [f] ((q, None, x.ss)::(q, None, y.ss)::(List.hd x.fs, None, f)::(List.hd y.fs, None, f)::(x.ts @ y.ts))
   | Concat(a, b) ->           
           let x = regexp_to_nfa a in
           let y = regexp_to_nfa b in
           make_nfa x.ss y.fs ((List.hd x.fs, None, y.ss) :: (x.ts @ y.ts))
   | Star(a) -> let q = next() in
           let f = next() in 
           let x = regexp_to_nfa a in
          make_nfa q [f] ((q, None, x.ss) :: (q, None, f) :: (f, None, q) :: (List.hd x.fs, None, f) :: (List.hd x.fs, None, x.ss) :: x.ts)
;;




exception IllegalExpression of string

(************************************************************************)
(* PARSER. You shouldn't have to change anything below this point *)
(************************************************************************)

(* Scanner code provided to turn string into a list of tokens *)

type token =
   Tok_Char of char
 | Tok_Epsilon
 | Tok_Union
 | Tok_Star
 | Tok_LParen
 | Tok_RParen
 | Tok_END

let re_var = Str.regexp "[a-z]"
let re_epsilon = Str.regexp "E"
let re_union = Str.regexp "|"
let re_star = Str.regexp "*"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"


let tokenize str =
 let rec tok pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_var s pos) then
       let token = Str.matched_string s in
       (Tok_Char token.[0])::(tok (pos+1) s)
	 else if (Str.string_match re_epsilon s pos) then
       Tok_Epsilon::(tok (pos+1) s)
	 else if (Str.string_match re_union s pos) then
       Tok_Union::(tok (pos+1) s)
	 else if (Str.string_match re_star s pos) then
       Tok_Star::(tok (pos+1) s)
     else if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tok (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tok (pos+1) s)
     else
       raise (IllegalExpression "tokenize")
   end
 in
 tok 0 str

(*
  A regular expression parser. It parses strings matching the
  context free grammar below.

   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let lookahead tok_list = match tok_list with
	[] -> raise (IllegalExpression "lookahead")
	| (h::t) -> (h,t)

let rec parse_S l =
	let (a1,l1) = parse_A l in
	let (t,n) = lookahead l1 in
	match t with
		Tok_Union -> (
		let (a2,l2) = (parse_S n) in
		(Union (a1,a2),l2)
		)
		| _ -> (a1,l1)

and parse_A l =
	let (a1,l1) = parse_B l in
	let (t,n) = lookahead l1 in
	match t with
	Tok_Char c ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| Tok_Epsilon ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| Tok_LParen ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| _ -> (a1,l1)

and parse_B l =
	let (a1,l1) = parse_C l in
	let (t,n) = lookahead l1 in
	match t with
	Tok_Star -> (Star a1,n)
	| _ -> (a1,l1)

and parse_C l =
	let (t,n) = lookahead l in
	match t with
   	  Tok_Char c -> (Char c, n)
	| Tok_Epsilon -> (Empty_String, n)
	| Tok_LParen ->
		let (a1,l1) = parse_S n in
		let (t2,n2) = lookahead l1 in
		if (t2 = Tok_RParen) then
			(a1,n2)
		else
			raise (IllegalExpression "parse_C 1")
	| _ -> raise (IllegalExpression "parse_C 2")

let string_to_regexp str =
	let tok_list = tokenize str in
	let (a,t) = (parse_S tok_list) in
	match t with
	[Tok_END] -> a
	| _ -> raise (IllegalExpression "string_to_regexp")

let string_to_nfa s = regexp_to_nfa (string_to_regexp s)

end

module Nfa : NFA = NfaImpl;;
