let default = 9999999;;

let falseInt = -1;

exception RunError of string;;

let rec mypow x y =
  if y = 0 then 1 else x * (mypow x (y-1))

let rec getValue env exp =
   match exp with

      | Id id -> let x = Hashtbl.find env id in if x = default then raise (RunError (id ^ " not initialized")) else x
      | Num a -> a
      | Sum (a, b) ->  (getValue env a) + (getValue env b)
      | Greater (a, b) ->  if (getValue env a) > (getValue env b) then 1 else falseInt
      | Equal (a, b) ->  if (getValue env a) = (getValue env b) then 1 else falseInt
      | Less (a, b) ->  if (getValue env a) < (getValue env b) then 1 else falseInt
      | Mult (a, b) ->  (getValue env a) * (getValue env b)
      | Pow (a, b) ->  mypow (getValue env a)  (getValue env b)
      | Paren a -> getValue env a
      | _ -> raise (RunError "not expression")


let rec eval env x=

    match  x with
       List [] -> env

      | List (h::t) -> let env = eval env h in  eval env (List t)
      | z -> 
      match z with
      | Fun (d, name, argulist, body) -> eval env body
      | Define (d, (Id id)) -> if Hashtbl.mem env id then raise (RunError  (id ^ " already defined")) else 
                                  Hashtbl.add env id default; env
      | Assign ((Id id), v) -> if Hashtbl.mem env id then Hashtbl.add env id (getValue env v) else raise (RunError  (id ^ " not defined")) ; env
      | If (a, b, c) -> if (getValue env a) = 1 then eval env b else eval env c
      | While (a, b) -> if (getValue env a) = 1 then let env = eval env b in eval env (While (a, b)) else env
      | Print a ->  print_int (getValue env a); print_endline ""; env
      |  _ -> env
;;
