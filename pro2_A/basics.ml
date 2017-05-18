(* CMSC 330 / Project 3 *)
(* Student: Jixiang Fan *)

(* Fill in the implementation and submit basics.ml *)

(* Part A: Simple functions *)

(* Implement a function head_divisor: int list -> bool that returns
    whether the head of the list given is a divisor of the second element *)
  let head_divisor l = 
      (*first chech null list return false*)
      match l with 
      [] -> false
      (*then check the list only has one element, return false*)
      | (h1::t1) ->
      match t1 with 
      []-> false
      (*get the second element*)
      | (h2::t2) -> 
      if h2 mod h1 = 0 then true else false
;;  


(* Implement a function tuple_addr: int * int * int -> int that returns
    the addition of the three numbers inside the tuple
    Hint: you don't need match *)

let tuple_addr nums = 
    match nums with 
    (h1,h2,h3) -> h1 + h2 + h3
;;

(* Implement a caddr_int : int list -> int that returns the second element
   of the list, if the list has two or more elements, and returns -1 if
   the list has zero or one elements. *)
let caddr_int l = 
    match l with 
    [] -> -1
    | (h1::(h2::_)) -> h2 
    | (h1::_) -> -1
;;

(* Part B: Simple curried functions. *)

(* A curried function is one that takes multiple arguments "one at a
      time". For example, the following function sub takes two arguments and
   computes their difference:

   let sub x y = x - y

   The type of this function is int -> int -> int. Technically, this
   says that sub is a function that takes an int and returns a
   function that takes another int and finally returns the answer,
   also an int. In other words, we could write

   sub 2 1

   and this will produce the answer 1. But we could also do something
   like this:

   let f = sub 2 in
   f 1

   and this will also produce 1. Notice how we call sub with only one
   argument, so it returns a function f that takes the second
   argument. In general, you can think of a function f of the type

   t1 -> t2 -> t3 -> ... -> tn

   as a function that takes n-1 arguments of types t1, t2, t3, ...,
   tn-1 and produces a result of type tn. Such functions are written
   with OCaml syntax

   let f a1 a2 a3 ... = body

   where a1 has type t1, a2 has type t2, etc.
*)

(* Implement a function mult_of_n: int -> int -> bool. Calling
   mutl_of_n x y returns true if x is a multiple of y, and false
   otherwise. For example, mult_of_n 5 5 = true and mult_of_n 21 5 =
   false. Note that mult_of_n x 0 = false for all x. *)
let mult_of_n x n = 
    if (n = 0) then false
    else x mod n  == 0 
;;    

(* Implement a function triple_it: 'a -> 'b -> 'c -> 'a*'b*'c. Calling
   triple_it on arguments x, y and z, should return a tuple with those
   three arguments, e.g., triple_it 1 2 3 = (1,2,3) *)
let triple_it x y z = 
  (x,y,z)
;;  

(* Write a function maxpair : int*int -> int*int -> int*int that takes
   two pairs of integers, and returns the pair that is larger,
   according to lexicographic ordering. For example, maxpair (1,2)
   (3,4) = (3,4), and maxpair (1,2) (1,3) = (1,3).
*)

let maxpair p1 p2 =
  match p1 with
    (x,y) -> 
  match p2 with
    (m,n) -> 
  max p1 p2
;;  


(* Part C: Recursive functions *)

(* Write a function power_of : int -> int -> bool that
   returns true if the second value is a power of the first
   otherwise false *)

let rec power_of x y = 
  if x = 0 && y = 0 then true 
  else if x = 0 && y = 1 then true 
  else if x = 0 && y != 0 then false  
  else if x = 1 && y != 1 then false
  else if y = 1 then true
  else if (y mod x != 0) then false
  else power_of x (y/x)
;;

(* Write a function prod : int list -> int. Calling prod l returns the
   product of the elements of l. The function prod should return 1 if
   the list is empty. *)

let rec prod l = 
    match l with 
    [] -> 1
    | (h1::t1) -> h1 * prod t1 
;;

(* Write a function unzip : ('a*'b) list -> ('a list)*('b
   list). Calling unzip l, where l is a list of pairs, returns a pair
   of lists with the elements in the same order. For example, unzip
   [(1, 2); (3, 4)] = ([1; 3], [2;4]) and unzip [] = ([],[]).
*)

let rec unzip l = 
    match l with
    [] ->([],[])
    | (h1x,h1y)::t1 -> 
    let m,n = unzip t1 in h1x::m,h1y::n 
;;


(* Write a function maxpairall : int*int list -> int*int. Calling
   maxpairall l returns the largest pair in the list l, according to
   lexicographic ordering. If the list is empty, it should return
   (0,0).  For example, maxpairall [(1,2);(3,4)] = (3,4) and
   maxpairall [(1,2);(2,1);(3,1)] = (3,1).
*)

let rec maxpairall l = 
    match l with
    [] -> (0,0)
    | (h1x,h1y)::t1 -> 
    if ((h1x,h1y) < maxpairall(t1)) then maxpairall(t1)
    else if ((h1x,h1y) > maxpairall(t1)) then (h1x,h1y)
    else (h1x,h1y)
;;      

(* Write a function addTail : 'a list -> 'a -> 'a list. Calling
   addTail l e returns a new list where e is appended to the back of
   l. For example, addTail [1;2] 3 = [1;2;3]. *)

let rec addTail l x = 
  match l with
  [] -> (x::[])
  | (h1::t1) -> (h1::addTail t1 x)
;;

(*
get_val x n
int list -> int -> int
element of list x at index n, or -1 if not found
  (indexes start at 0)
Example: get_val [5;6;7;3] 1 => 6
*)
let rec get_val x n = 
  match x with
  [] -> -1
  | (h1::t1) -> 
  if (n < 0) then -1
  else if (n != 0) then (get_val t1 (n-1)) 
  else h1 
;;  

(*
get_vals x y
int list -> int list -> int list
list of elements of list x at indexes in list y,
[] if any indexes in y are outside the bounds of x;
elements must be returned in order listed in y
Example: get_vals [5;6;7;3] [2;0] => [7;5]
*)
let rec get_vals b n = 
  match n with
  [] -> []
  | (h1::t1) -> (get_val b h1)::(get_vals b t1)
;;  

(*
list_swap_val b u v
'a list -> 'a -> 'a -> 'a list
list b with values u,v swapped
(change value of multiple occurrences of u and/or v, if found, and
change value for u even if v not found in list, and vice versa )
Example: list_swap_val [5;6;7;3] 7 5 => [7;6;5;3]
*)
let rec list_swap_val b u v = 
  match b with
  [] -> []
  | (h1::t1) -> 
  if (h1 == u) then (v::(list_swap_val t1 u v))
  else if (h1 == v) then (u::(list_swap_val t1 u v))
  else (h1::(list_swap_val t1 u v))
;;

(* Write a function index : 'a list -> 'a -> int. Calling index l e
   returns the index in l of the rightmost occurrence of e, or -1 if e
   is not present. The first element has index 0. For example, index
   [1;2;2] 1 = 0 and index [1;2;2;3] 2 = 2 and index [1;2;3] 4 = -1.

   Hint: it's easiest to write a helper function, but you can also do
   it without one.
*)

let rec index l e = 
    let rec helper l position empty=
    match l with 
    (h1::t1) ->  (helper t1 (position+1) (if h1 == e then position else empty))
    | [] -> empty
    in
    helper l 0 (-1)
;;    

(*
distinct x
'a list -> 'a list
return list of distinct members of list x
*)
let rec delete l e = 
    match l with
      [] -> []
  (* first get the first element in the list, check weather it was the selected number 
  , if it was not, store it. otherwise recursive*)
      | (h1::t1) -> 
      if (h1 != e) then (h1::(delete t1 e)) else (delete t1 e)
;;

let rec distinct l = 
  match l with
  [] -> []
  | (h1::t1) -> (h1::(distinct(delete t1 h1)))
;;


(*
find_new x y
'a list -> 'a list -> 'a list
list of members of list x not found in list y
maintain relative order of elements in result
Example: find_new [4;3;7] [5;6;5;3] => [4;7]
*)

let rec found l e =
  match l with 
  [] -> false
  | (h1::t1) ->
  if (h1 != e) then (found t1 e) else true
;;

let rec find_new x y = 
  match x with 
  [] -> []
  (* first get the first element in the x list, check weather y list contain it
  if it has then means it was not the number we want to return, so Recursive. 
  otherwise, store the number in the list and sort the rest list *)
  | (h1::t1) ->
  if (found y h1 == true) then (find_new t1 y)
  else (h1::(find_new t1 y)) 
;;

(*
  power_list l
  int list -> bool
  returns true if each consecutive value in the list is a
  power of the previous element in the list
*)

let rec power_list l = 
  match l with
    [] -> true
    | (_::[]) ->true
    | h1::h2::t1 -> (power_of h1 h2) && (power_list (h2::t1))
;;
