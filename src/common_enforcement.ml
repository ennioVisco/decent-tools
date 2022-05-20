open Alphabetevent
open List
open Ltl
open Trace

exception Enforcement_Input_Error of ltl
exception Enforcement_Error

(* Image of the partial events in the Temporal Correction Log*)
type image = ltl * int

let string_rep_image (image_pair: image) =
  "( " ^ (string_rep_simple (fst image_pair)) ^ ", " ^ (string_of_int (snd image_pair)) ^ " )"

let string_rep_image_min (image_pair: image) =
  "(" ^ string_rep_min (fst image_pair) ^ "," ^ string_of_int (snd image_pair) ^ ")"


(* Expansion function, rewrites the formula to separate the present and future obligations *)
let rwT (formula: ltl) =
  let simp_f = simp formula in
  let rec rwT_rec (simp_f: ltl) =
    match simp_f with
        True | False | Var _ | Next _ -> simp_f
      | Neg f -> Neg (rwT_rec f)
      | Or(f1, f2) -> Or(rwT_rec f1, rwT_rec f2)
      | And(f1, f2) -> And(rwT_rec f1, rwT_rec f2)
      | Glob f -> And(rwT_rec f, Next simp_f)
      | Ev f -> Or(rwT_rec f, Next simp_f)
      | Until (f1, f2) | Wuntil (f1, f2) -> Or(rwT_rec f2, And(rwT_rec f1, Next simp_f))
      (** No support for past-time LTL yet and SHARP should never be seen here,
          simp removes implications and equivalences **)
      | _ -> raise (Enforcement_Input_Error(formula))
  
  in rwT_rec simp_f

(* Computes the distance between two events w.r.t. the given list of atomic proposition "apr" *)
let rec distance (e1: event) (e2: event) (apr: string list) =
  match apr with
      hd::tl -> if (List.mem hd e1) <> (List.mem hd e2) then 1 + (distance e1 e2 tl)
                                                        else distance e1 e2 tl  
    | _ -> 0

(* Adds an atomic proposition p to the given list of observation *)
let rec add_atomic_prop (p: string) (observ_list: string list list) =
  match observ_list with
      hd::tl -> (add_atomic_prop p tl) @ [ p::hd; hd ] 
    | _ -> []

(* Generates a list of all the possible local observations from a given list of atomic
  propositions *)
let rec generate_local_observations (apr: string list) =
  match apr with
      hd::tl -> add_atomic_prop hd (generate_local_observations tl)
    | _ -> [[]]

let rec update_local_observations (p: string) (observ_list: string list list) =
  match observ_list with
      hd::[] -> [p::hd]
    | hd::tl -> [p::hd] @ (update_local_observations p tl)
    | [] -> []

(* As the enforcer only rewrite the formula using "relevant" atomic propositions (that is, only the atomic prop
  they can locally observe and that have not been evaluated in the formula yet), we need to add the ones that are
  included in the event but not in apr.
  Intuitively, these atomic propositions are not included in the formula that we are currently enforcing and
  therefore, we have no reason to modify their values. *)
let rec add_unmodified_ap (sys_event: event) (apr: string list) (observ_list: string list list) =
  match sys_event with
      hd::tl -> (
        if List.mem hd apr = false then 
          let new_observ_list = update_local_observations hd observ_list in
          add_unmodified_ap tl apr new_observ_list
        else
          add_unmodified_ap tl apr observ_list
      )
    | _ -> observ_list

let compute_avg_msg_size (msg_size: int list) (nb_msg: int list) =
  let sum = ref 0.0 in
  let n = ref 0.0 in

  let rec compute_avg_msg_size_rec (msg_size: int list) (nb_msg: int list) =
    match msg_size, nb_msg with
        hd_size::tl_size, hd_nb::tl_nb -> (
          let avg_msg_size = if hd_nb <> 0 then (float_of_int hd_size) /. (float_of_int hd_nb) else 0.0 in
          if avg_msg_size <> 0.0 then (sum := !sum +. avg_msg_size; n := !n +. 1.0);    
          compute_avg_msg_size_rec tl_size tl_nb
        )
      | _ -> if !n <> 0.0 then !sum /. !n else 0.0
    
  in compute_avg_msg_size_rec msg_size nb_msg

let compute_avg_tcl_size (tcl_size: int list) (squared: bool) =
  let sum = ref 0.0 in
  let n = ref 0.0 in

  let rec compute_avg_tcl_size_rec (tcl_size: int list) =
    match tcl_size with
        hd::tl -> (
          if hd <> 0 then (
            let hd_f = float_of_int hd in
            if (squared) then
              sum := !sum +. (hd_f ** 2.)
            else
              sum := !sum +. hd_f;
            n := !n +. 1.0
          );
          compute_avg_tcl_size_rec tl
        )
      | _ -> ()
      
  in compute_avg_tcl_size_rec tcl_size;
  if squared then
    (!sum, !n)
  else if !n <> 0.0 then
    (!sum /. !n, !n)
  else
    (0.0, !n)


(* Function updating the formula with a local event and a local alphabet *)
let rec rwi (f: ltl) (alpha: alphabet) (e: event) =
  let rec rwi_rec (f: ltl) =
    match f with
        True | False -> f
      | Var x -> if List.mem x e then True else 
                    if List.mem x alpha then False else Var x
      | Neg f -> Neg (rwi_rec f)
      | And(f1, f2) -> And(rwi_rec f1, rwi_rec f2)
      | Or(f1, f2) -> Or(rwi_rec f1, rwi_rec f2)
      | _ -> f
  
  in rwi_rec f

let rec remove_next (f: ltl) = 
  match f with
      True | False | Var _ -> f
    | Neg f1 -> Neg (remove_next f1)
    | And(f1, f2) -> And((remove_next f1), (remove_next f2))
    | Or(f1, f2) -> Or((remove_next f1), (remove_next f2))
    | Glob f1 -> Glob (remove_next f1)
    | Ev f1 -> Ev (remove_next f1)
    | Next f1 -> f1
    | Until(f1, f2) -> Until((remove_next f1), (remove_next f2))
    | Wuntil(f1, f2) -> Wuntil((remove_next f1), (remove_next f2))
    | _ -> raise (Enforcement_Input_Error f) 

(* Returns a list of the atomic propositions contained in "formula" *)
let rec ap_formula (formula: ltl) =
  match formula with
      True | False -> []
    | Var x -> [x]
    | Neg f -> ap_formula f
    | And(f1, f2) | Or(f1, f2) -> (ap_formula f1) @ (ap_formula f2)
    (* In this case, as we do not split present and future, we can encounter temporal modalities
      but we do not explore the subtree because we only care about present obligations *)
    | _ -> []

(* Function returning un-evaluated atomic propositions in the tcl that the current enforcer can
  update *)
let relevant_ap (i: image list) (alpha: alphabet) =
  let rec remaining_ap (i: image list) =
    match i with
        hd::tl -> (ap_formula (fst hd)) @ (remaining_ap tl)
      | _ -> []

  in
  let ap_remaining = sort_uniq compare (remaining_ap i) in
  let sorted_alpha = fast_sort compare alpha in
  (* Should be in O(n) as both lists are sorted (so the total function should be in
    O(n * log(n)) because of the sort which is better than a naive way of doing it) *)
  let rec intersect (ap_r: string list) (alpha: string list) =
    match ap_r, alpha with
      | [], _ | _, [] -> []
      | hd1::tl1, hd2::tl2 when hd1 = hd2 -> [hd1] @ (intersect tl1 tl2)
      | hd1::tl1, hd2::tl2 -> if (compare hd1 hd2) < 0 then (intersect tl1 alpha)
                                                        else (intersect ap_r tl2) 
  (* The "relevant" list of atomic propositions is the intersection between the local alphabet
  and the un-evaluated atomic propositions *)
  in intersect ap_remaining sorted_alpha