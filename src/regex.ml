open Regex_base


(*fonction qui ajoute les elements de b dans a*)
let rec append a b =
  match a with
  | [] -> b
  | e :: reste -> e :: append reste b

let rec repeat n l =
  if n = 0 then []
  else 
  if n = 1 
    then l
  else
     (append l (repeat (n-1) l)) (*on rajoute n-1 fois les elements à la liste avec append (puisque déjà présent 1 fois dans l)*)

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
  | Star exp -> is_empty exp   (*l'expression e* est finie seulement si e est vide*)


let rec is_null ll = 
  match ll with 
  | [] -> true
  | tete :: reste -> 
    match tete with 
    | [] -> is_null reste
    | _ -> false 

let rec product_aux l ll =
  match l with 
  |[] -> if is_null ll then [] else ll
  |_ -> 
    match ll with 
    | [] -> []
    | tete :: reste -> (append l tete) :: product_aux l reste


let rec product l1 l2 =
  match l1 with 
  | [] -> product_aux [] l2
  | [tete] -> product_aux tete l2
  | tete :: reste -> append (product_aux tete l2) (product reste l2) 
  

let create_combinations alphabet =
    List.map (fun a -> [a]) alphabet

let rec enumerate alphabet e =
  match e with
  | Eps -> Some [[]]  (* Le langage contenant seulement la chaîne vide *)
  | Base a -> if List.mem a alphabet then Some [[a]] else Some [] 
  | Star exp -> (*si le langage est fini on peut retourner quelque chose sinon None *)
    if is_empty exp || null exp then Some [[]] else None
  | Joker -> Some (create_combinations alphabet) (*on applique la fonction avec chaque lettre de l'alphabet puisque le joker contient n'importe laquelle*)
  | Alt (e1, e2) -> 
      (match (enumerate alphabet e1, enumerate alphabet e2) with
      | (Some l1, Some l2) -> Some (append l1 l2)
      | _ -> None) (*Si l'un des appels récursifs retourne None,cela signifie qu'il y a une expression infine. Alors Alt(e1,e2) l'est aussi*)
  |Concat (e1,e2) ->
    (match (enumerate alphabet e1, enumerate alphabet e2) with
    | (Some l1, Some l2) -> Some (product l1 l2) (*On concatene les elements*)
    | _ -> None (*meme cas que Alt*))

    
let rec alphabet_expr e =
  match e with 
  | Eps -> []
  | Base a -> [a] 
  | Joker -> []
  | Concat (e1, e2) -> union_sorted (alphabet_expr e1 )  (alphabet_expr e2 )
  | Alt (e1, e2) -> union_sorted (alphabet_expr e1 ) (alphabet_expr e2 )
  | Star exp -> alphabet_expr exp

type answer =
  Infinite | Accept | Reject

let rec list_match l1 l2 = 
  match l1,l2 with 
  | ([],[]) -> true 
  | ([],_) -> false 
  | (_,[]) -> false 
  | (tete1 :: reste1 ,tete2 :: reste2) -> 
    if tete != tete then 
      false 
    else 
      list_match reste1 reste2




let rec list_l_in_ll l ll = 
  match ll with 
  | [] -> false 
  | tete :: reste -> 
    if list_match l tete then 
      true
    else
      list_l_in_ll l reste 

let accept_partial e w =
  let langage_reconnu = enumerate w e in 
  match langage_reconnu with 
  | None -> Infinite
  | Some l -> if list_l_in_ll then Accept else Reject 
