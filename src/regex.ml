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
  failwith "À compléter"

let rec null e =
  failwith "À compléter"

let rec is_finite e =
  failwith "À compléter"

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
