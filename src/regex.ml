open Regex_base

let rec repeat n l =
  if n = 0 then []
  else 
  if n = 1 
    then l
  else
    l@(repeat (n-1) l)

let rec expr_repeat n e =
  if n = 0 then Eps
    else
  Concat (e,(expr_repeat (n-1) e))

let rec is_empty e =
  match e with
  | Eps -> true
  | Base a -> false
  | Joker -> false
  | Concat (e1,e2) -> if is_empty e1 then is_empty e2 else false
  | Alt(e1,e2) -> if is_empty e1 then is_empty e2 else false
  | Star exp -> is_empty exp

let rec null e =
  match e with
  | Eps -> true
  | Base a -> false
  | Joker -> false
  | Concat (e1,e2) -> if null e1 then null e2 else false
  | Alt(e1,e2) -> if null e1 then true else null e2
  | Star exp -> true

let rec is_finite e =
  match e with
  | Eps -> true
  | Base a -> true
  | Joker -> true
  | Concat (e1,e2) -> if is_finite e1 then is_finite e2 else false
  | Alt(e1,e2) -> if is_finite e1 then is_finite e2 else false
  | Star exp -> if is_empty exp then true else false   (*l'expression e* est finie seulement si e est vide*)

let product l1 l2 =
  failwith "À compléter"

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
