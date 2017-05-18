 (*
	CMSC330 Fall 2016
	This ocaml code reads a C code and properly indents it
	
	compile for debug:
		ocamlc -g Str.cma smallc.ml 
	
	@author: Anwar Mamat
	@date: 10/15/2016
*)

#load "str.cma"

type data_type =
	|Type_Int
;;

(* Use this as your abstract syntax tree *)

type ast =
  | Id of string
  | Num of int
  | Define of data_type * ast
  | Assign of ast * ast
  | List of ast list
  | Fun of data_type * string * ast * ast   (* return type * function name * argument list * statement list *)
  | Sum of ast * ast
  | Greater of ast * ast
  | Equal of ast * ast
  | Less of ast * ast
  | Mult of ast * ast
  | Pow of  ast * ast
  | Print of ast
  | If of ast * ast * ast	(* cond * if brach * else branch *)
  | While of ast * ast
  | Paren of ast
  
;;

type token =
 | Tok_Id of string
 | Tok_Num of int
 | Tok_String of string
 | Tok_Assign
 | Tok_Greater
 | Tok_Less
 | Tok_Equal
 | Tok_LParen
 | Tok_RParen
 | Tok_Semi
 | Tok_Main
 | Tok_LBrace
 | Tok_RBrace
 | Tok_Int 
 | Tok_Float
 | Tok_Sum
 | Tok_Mult
 | Tok_Pow
 | Tok_Print
 | Tok_If
 | Tok_Else
 | Tok_While
 | Tok_END
 
(* tokens *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lbrace = Str.regexp "{"
let re_rbrace = Str.regexp "}"
let re_assign = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_equal = Str.regexp "=="
let re_semi = Str.regexp ";"
let re_int = Str.regexp "int"
let re_float = Str.regexp "float"
let re_printf = Str.regexp "printf"
let re_main = Str.regexp "main"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_num = Str.regexp "[-]?[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_whitespace = Str.regexp "[ \t\n]"
let re_add = Str.regexp "+"
let re_mult = Str.regexp "*"
let re_pow = Str.regexp "\\^"
let re_if = Str.regexp "if"
let re_else = Str.regexp "else"
let re_while = Str.regexp "while"


exception Lex_error of int
exception Parse_error of int ;;
exception IllegalExpression of string

let tokenize s =
 let rec tokenize' pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_add s pos) then
       Tok_Sum::(tokenize' (pos+1) s)
     else if (Str.string_match re_mult s pos) then
       Tok_Mult::(tokenize' (pos+1) s)
     else if (Str.string_match re_equal s pos) then
       Tok_Equal::(tokenize' (pos+2) s)
     else if (Str.string_match re_if s pos) then
       Tok_If::(tokenize' (pos+2) s)
     else if (Str.string_match re_else s pos) then
       Tok_Else::(tokenize' (pos+4) s)    
     else if (Str.string_match re_while s pos) then
       Tok_While::(tokenize' (pos+5) s)       
	else if (Str.string_match re_pow s pos) then
       Tok_Pow::(tokenize' (pos+1) s)
    else if (Str.string_match re_printf s pos) then
       Tok_Print::tokenize' (pos+6) s
    else if (Str.string_match re_lbrace s pos) then
       Tok_LBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_rbrace s pos) then
       Tok_RBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_assign s pos) then
       Tok_Assign::(tokenize' (pos+1) s)
    else if (Str.string_match re_greater s pos) then
       Tok_Greater::(tokenize' (pos+1) s)
    else if (Str.string_match re_less s pos) then
       Tok_Less::(tokenize' (pos+1) s)
    else if (Str.string_match re_semi s pos) then
       Tok_Semi::(tokenize' (pos+1) s)
    else if (Str.string_match re_int s pos) then
       Tok_Int::(tokenize' (pos+3) s)
    else if (Str.string_match re_float s pos) then
       Tok_Float::(tokenize' (pos+5) s)
    else if (Str.string_match re_main s pos) then
       Tok_Main::(tokenize' (pos+4) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Id token)::(tokenize' new_pos s)
     else if (Str.string_match re_string s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       let tok = Tok_String (String.sub token 1 ((String.length token)-2)) in
       tok::(tokenize' new_pos s)
     else if (Str.string_match re_num s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Num (int_of_string token))::(tokenize' new_pos s)
     else if (Str.string_match re_whitespace s pos) then
       tokenize' (Str.match_end ()) s
     else
       raise (Lex_error pos)
   end
 in
 tokenize' 0 s
 
 
 (* C Grammar *)
 (* 
 
 basicType-> 'int'
  mainMethod-> basicType 'main' '(' ')' '{' methodBody '}'
  methodBody->(localDeclaration | statement)*
  localDeclaration->basicType ID ';'
  statement->
    whileStatement
    |ifStatement
    |assignStatement
    |printStatement
  
  assignStatement->ID '=' exp ';'
  ifStatement -> 'if' '(' exp ')'  '{' ( statement)* '}'  ( 'else' '{'( statement)* '}')?
  whileStatement -> 'while''(' exp ')' '{'(statement )*'}'
  printStatement->'printf' '(' exp ')' ';'
  exp -> additiveExp (('>'  | '<'  | '==' ) additiveExp )*
  additiveExp -> multiplicativeExp ('+' multiplicativeExp)*
  multiplicativeExp-> powerExp ( '*' powerExp  )*
  powerExp->primaryExp ( '^' primaryExp) *
  primaryExp->'(' exp ')' | ID 
  ID->( 'a'..'z' | 'A'..'Z') ( 'a'..'z' | 'A'..'Z' | '0'..'9')*
  WS-> (' '|'\r'|'\t'|'\n') 



*)

(*----------------------------------------------------------
  function lookahead : token list -> (token * token list)
	Returns tuple of head of token list & tail of token list
*)

let lookahead tok_list = match tok_list with
        [] -> raise (IllegalExpression "lookahead")
        | (h::t) -> (h,t)
;;        


let check tok expect = 
    if tok = expect 
    then true 
    else raise (IllegalExpression "wrong token")

let checkNext tokList expect = 
    let tok, rest = lookahead tokList in 
    if tok = expect 
    then rest 
    else raise (IllegalExpression "wrong token")

let rec parse_localDeclaration lst =
    let lst = checkNext lst Tok_Int in 
    let h, lst = lookahead lst in  
    match h with
      | (Tok_Id id) -> let lst = checkNext lst Tok_Semi in Define(Type_Int,Id id), lst
      | _ -> raise (IllegalExpression "unexpect token")
(* let rec parse_exp lst = (Id "ToDo",lst) *)

and parseMethodBody lst =
    let h, rest = lookahead lst in 
    match h with
      | Tok_Int -> let x, lst = parse_localDeclaration lst in let y, lst = parseMethodBody lst in x::y, lst
      | Tok_While -> let x, lst = parse_whileStatement lst in let y, lst = parseMethodBody lst in x::y, lst
      | Tok_If -> let x, lst = parse_ifStatement lst in let y, lst = parseMethodBody lst in x::y, lst
      | Tok_Id _ -> let x, lst = parse_assignStatement lst in let y, lst = parseMethodBody lst in x::y, lst
      | Tok_Print -> let x, lst = parse_printStatement lst in let y, lst = parseMethodBody lst in x::y, lst
      | _ -> [], lst

and parse_methodBody lst = 
    let al, lst = parseMethodBody lst in (List al), lst


(* statement->
    ifStatement
    |whileStatement
    |assignStatement
    |printStatement
  
  assignStatement->ID '=' exp ';'
  ifStatement -> 'if' '(' exp ')'  '{' ( statement)* '}'  ( 'else' '{'( statement)* '}')?
  whileStatement -> 'while''(' exp ')' '{'(statement )*'}'
  printStatement->'printf' '(' exp ')' ';' *)

(*| If of ast * ast * ast  cond * if brach * else branch *)


 and parse_ifStatement lst =
   let lst = checkNext lst Tok_If in 
   let lst = checkNext lst Tok_LParen in 
   let x, lst = parse_exp lst in 
   let lst = checkNext lst Tok_RParen in 
   let lst = checkNext lst Tok_LBrace in 
   let y, lst = parse_statements lst in 
   let lst = checkNext lst Tok_RBrace in 
   let h, r = lookahead lst in 
   if h = Tok_Else then 
       let lst = r in 
       let lst = checkNext lst Tok_LBrace in 
       let z, lst = parse_statements lst in 
       let lst = checkNext lst Tok_RBrace in 
       If (x, y, z) , lst
  else
      If (x, y, (List [])) , lst


(* whileStatement -> 'while''(' exp ')' '{'(statement )*'}' *)
(* While of ast * ast *)
and parse_whileStatement lst = 
   let lst = checkNext lst Tok_While in 
   let lst = checkNext lst Tok_LParen in 
   let x, lst = parse_exp lst in 
   let lst = checkNext lst Tok_RParen in 
   let lst = checkNext lst Tok_LBrace in 
   let y, lst = parse_statements lst in 
   let lst = checkNext lst Tok_RBrace in 
   While (x, y) , lst


 (*printStatement->'printf' '(' exp ')' ';' *)
 (* Print of ast *)
 and parse_printStatement lst =
    let lst = checkNext lst Tok_Print in 
    let lst = checkNext lst Tok_LParen in 
    let x, lst = parse_exp lst in 
    let lst = checkNext lst Tok_RParen in 
    let lst = checkNext lst Tok_Semi in 
    (Print x) , lst


 and parse_assignStatement lst =
    let h, lst = lookahead lst in 
    match h with
      Tok_Id id -> 
      let lst = checkNext lst Tok_Assign in 
      let y, lst = parse_exp lst in 
      let lst = checkNext lst Tok_Semi in  Assign ((Id id), y), lst
      | _ -> raise (IllegalExpression ("unexpected token"))




and parseStatements lst = 
   let h, rest = lookahead lst in 
   match h with
    | Tok_While -> let x, lst = parse_whileStatement lst in let y, lst = parseStatements lst in x::y, lst
    | Tok_If -> let x, lst = parse_ifStatement lst in let y, lst = parseStatements lst in x::y, lst
    | Tok_Id _ -> let x, lst = parse_assignStatement lst in let y, lst = parseStatements lst in x::y, lst
    | Tok_Print -> let x, lst = parse_printStatement lst in let y, lst = parseStatements lst in x::y, lst
    | _ -> [], lst

and parse_statements lst = let al, lst = parseStatements lst in (List al), lst



and parse_exp lst = 
   let x, lst = parse_additiveExp lst in 
   let h, rest = lookahead lst in 
   match h with
     Tok_Less -> let y, lst = parse_exp rest in Less (x, y), lst
     | Tok_Equal -> let y, lst = parse_exp rest in Equal (x, y), lst
     | Tok_Greater -> let y, lst = parse_exp rest in Greater (x, y), lst
     | _ -> x, lst

 (* exp -> additiveExp (('>'  | '<'  | '==' ) additiveExp )*
  additiveExp -> multiplicativeExp ('+' multiplicativeExp)*
  multiplicativeExp-> powerExp ( '*' powerExp  )*
  powerExp->primaryExp ( '^' primaryExp) *
  primaryExp->'(' exp ')' | ID 
  ID->( 'a'..'z' | 'A'..'Z') ( 'a'..'z' | 'A'..'Z' | '0'..'9')*
  WS-> (' '|'\r'|'\t'|'\n')  *)

and parse_additiveExp lst =
    let x, lst = parse_multiplicativeExp lst in 
    let h, rest = lookahead lst in 
    match h with
      Tok_Sum -> let y, lst = parse_additiveExp rest in Sum (x, y) , lst
      | _ -> x, lst

and parse_multiplicativeExp lst =   
    let x, lst = parse_powerExp lst in 
    let h, rest = lookahead lst in 
    match h with
      Tok_Mult -> let y, lst = parse_multiplicativeExp rest in Mult (x, y) , lst
      | _ -> x, lst

and parse_powerExp lst =  
    let x, lst = parse_primaryExp lst in 
    let h, rest = lookahead lst in 
    match h with
      Tok_Pow -> let y, lst = parse_powerExp rest in Pow (x, y), lst
      | _ -> x, lst

and parse_primaryExp lst = 
    let  h, lst = lookahead lst in 
    match h with 
      Tok_LParen -> let x, lst = parse_exp lst in let lst = checkNext lst Tok_RParen in  (Paren x), lst
      | Tok_Id s -> (Id s), lst
      | Tok_Num n -> (Num n), lst
      | _  -> raise (IllegalExpression "unexpect token")


(* -------------- Your Code Here ----------------------- *)
let rec parse_Function lst = 
  (*  let h, rest = lookahead lst in 
   let x = check h Tok_Int *)
   let lst = checkNext lst Tok_Int in
   let lst = checkNext lst Tok_Main in 
   let lst = checkNext lst Tok_LParen in 
   let lst = checkNext lst Tok_RParen in 
   let lst = checkNext lst Tok_LBrace in 
   let x, lst = parse_methodBody lst in 
   let lst = checkNext lst Tok_RBrace in 
   Fun(Type_Int, "main" , List([]), x), lst
(* ------------------------------------------------------*)





exception Error of int ;;




let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let tok_to_str t = ( match t with
          Tok_Num v -> string_of_int v
        | Tok_Sum -> "+"
        | Tok_Mult ->  "*"
        | Tok_LParen -> "("
        | Tok_RParen -> ")"
		| Tok_Pow->"^"
        | Tok_END -> "END"
        | Tok_Id id->id
		| Tok_String s->s
		| Tok_Assign->"="
		 | Tok_Greater->">"
		 | Tok_Less->"<"
		 | Tok_Equal->"=="
		 | Tok_Semi->";"
		 | Tok_Main->"main"
		 | Tok_LBrace->"{"
		 | Tok_RBrace->"}"
		 | Tok_Int->"int" 
		 | Tok_Float->"float"
		 | Tok_Print->"printf"
		 | Tok_If->"if"
		 | Tok_Else->"else"
		 | Tok_While-> "while"
    )

let print_token_list tokens =
	print_string "Input token list = " ;
	List.iter (fun x -> print_string (" " ^ (tok_to_str x))) tokens;
	print_endline ""
;;
	



(* type ast =
  | Id of string
  | Num of int
  | Define of data_type * ast
  | Assign of ast * ast
  | List of ast list
  | Fun of data_type * string * ast * ast   (* return type * function name * argument list * statement list *)
  | Sum of ast * ast
  | Greater of ast * ast
  | Equal of ast * ast
  | Less of ast * ast
  | Mult of ast * ast
  | Pow of  ast * ast
  | Print of ast
  | If of ast * ast * ast (* cond * if brach * else branch *)
  | While of ast * ast
  | Paren of ast
  
;; *)


(* -------------- Your Code Here ----------------------- *)

let rec pretty_print pos x=

    match  x with
      List [] -> ()
      | List (h::t) -> pretty_print pos h; pretty_print pos (List t)
      | z ->

    print_string (String.make pos '_') ;
	  match z with
    | Fun (d, name, argulist, body) -> print_endline "int main(){";  pretty_print (pos + 4)  body; print_endline "}"
    | Define (d, (Id id)) -> print_string ("int " ^ id ^ ";\n") 
    | Assign ((Id id), x) -> print_string (id ^ " = ") ; pretty_print 0 x; print_endline ";"
    | Sum (a, b) ->  pretty_print 0 a; print_string " + "; pretty_print 0 b
    | Greater (a, b) ->  pretty_print 0 a; print_string " > "; pretty_print 0 b
    | Equal (a, b) ->  pretty_print 0 a; print_string " == "; pretty_print 0 b
    | Less (a, b) ->  pretty_print 0 a; print_string " < "; pretty_print 0 b
    | Mult (a, b) ->  pretty_print 0 a; print_string " * "; pretty_print 0 b
    | Pow (a, b) ->  pretty_print 0 a; print_string " ^ "; pretty_print 0 b
    | Print a ->  print_string "printf("; pretty_print 0 a; print_string ");\n"
    | Paren a -> print_string "("; pretty_print 0 a; print_string ")"
    | Id id -> print_string id
    | Num a -> print_int a
    | If (a, b, List []) -> print_string "if("; pretty_print 0 a; print_string "){\n"; pretty_print (pos + 4)  b; 
        print_string (String.make pos '_') ; print_string "}\n"
    | If (a, b, c) -> print_string "if("; pretty_print 0 a; print_string "){\n"; pretty_print (pos + 4)  b; 
        print_string (String.make pos '_') ; print_string "}else{\n"; pretty_print (pos + 4) c; 
        print_string (String.make pos '_'); print_string "}\n"
    | While (a, b) -> print_string "while("; pretty_print 0 a; print_string "){\n"; pretty_print (pos + 4)  b; 
        print_string (String.make pos '_') ; print_string "}\n"
    |  _ -> ()
;;

(* ----------------------------------------------------- *)


(*
you can test your parser and pretty_print with following code 
*)

(*

let prg1 = read_lines "main.c";;
let code = List.fold_left (fun x y->x^y) "" prg1;;	
let t = tokenize code;;
let (a,b)=parse_Function t;;
*)
