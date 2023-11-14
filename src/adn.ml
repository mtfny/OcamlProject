type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
  | A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let rec explode_aux str l i = 
  if i = String.length str then 
    l
  else 
    explode_aux str (str.[i]::l) (i+1)


let explode (str : string) : char list =
  List.rev (explode_aux str [] 0)


(* conversions *)
let base_of_char (c : char) : base =
  match c with 
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | _ -> WC 
;;

let dna_of_string (s : string) : base list = (*on on converti la chaine en tableau puis on convertit les caractères en base nucléique*)
  List.map base_of_char (explode s)


let rec string_of_dna (seq : dna) : string =
  match seq with
  | [] -> ""
  |base :: rest -> (string_of_base base)^(string_of_dna rest)



(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match (slice,list) with
  |([],_) -> Some list
  |(e::rest, []) -> None
  |(e1::rest1,e2::rest2) -> if e1 = e2 then cut_prefix rest1 rest2 else None
  

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)

(*
  Fonction auxilière pour first_occ   
*)
let rec first_occ_aux (slice : 'a list) (before : 'a list) (after : 'a list): ('a list * 'a list) option =
  match slice, after with 
  | [], _ -> Some (slice, after)
  |_ , [] -> None
  | _ , (tete :: reste) -> (
    let tmp = cut_prefix slice after in
    match tmp with 
    | None -> first_occ_aux slice (tete::before) reste 
    | Some l -> Some ((List.rev before), l) 
  )


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
    first_occ_aux slice [] list 
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let l1 = first_occ start list in
  match l1 with 
  | None -> []
  | Some (before, after) -> 
    let l2 = first_occ stop after in
    match l2 with
    | None -> [] 
    | Some(before_bis, after_bis) -> before_bis :: (slices_between start stop after_bis)

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) =
  let dna_list = explode (string_of_dna dna) in
  slices_between [A; T; G] [T; A; A] (List.map base_of_char dna_list)

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)

(*Fonctions auxiliares pour consensus*)
let rec counter (list : 'a list) (base : base) : int = 
  match list with
  | [] -> 0
  |base :: rest -> 1 + counter rest base

let max (a: int) (b:int) : () int =
  if a = b then None
  else
  if a<b then 
    b 
  else a

let consensus (list : 'a list) : 'a consensus =
  let c1 = (max (counter A) (counter C)) in 
  let c2 = (max (counter G) (counter T)) in
  if c1 = -1 || c2 = -1 then No_consensus
  else if c1 < c2 then
    if c2 = List.length list then Full

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  failwith "À compléter"

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
