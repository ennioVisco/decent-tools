open Ltl
open Dltl
type truth_value = True | False | Undeter

(** Conversion of Ltl to truth-value **)
let to_truth_value (f:ltl) : truth_value =
  match f with
    | True -> True
    | False -> False  
    | _ -> Undeter
    
let stringrep_truthvalue (v:truth_value) : string =
  match v with
    | True -> "tt"
    | False -> "ff"
    | Undeter -> "?"

exception End_of_Monitoring of int * truth_value

let is_monitoring_verbose : bool ref = ref false
  
let monitoring_message (msg:string) : unit =
  if (!is_monitoring_verbose)
  then
    print_endline (msg)


let count_needed_progressions (f:ltl) : int =
  let rec count (f:ltl) =
    match f with
      | Or (f1,f2) | And(f1,f2) | Until (f1,f2) | Xor (f1,f2) -> 2 + count f1 + count f2
      | Neg(f1) | Next(f1) | Glob (f1) | Ev (f1) -> 1 + count f1
      | Previous e -> 2
      | _ -> 1
  in
  count f

let count_needed_progressions_dltl (f:dltl) : int =
  let rec count_dltl (f:dltl) : int =
    match f with
      | Or (f1,f2) | And(f1,f2) | Until (f1,f2) | Xor (f1,f2) -> 2 + count_dltl f1 + count_dltl f2
      | Neg(f1) | Next(f1) | Glob (f1) | Ev (f1) -> 1 + count_dltl f1
      | _ -> 1
  in
  count_dltl f
