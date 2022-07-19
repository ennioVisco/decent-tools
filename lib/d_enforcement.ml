open Alphabetevent
open Common_enforcement
open List
open Ltl
open Trace       
open Utils

module PartialEvent = struct type t = d_event let compare = (compare compare_event) end
module Tcl = Map.Make(PartialEvent)
type tcl = image Tcl.t

let string_rep_image (image_pair: image) =
  "( " ^ (string_rep_simple (fst image_pair)) ^ ", " ^ (string_of_int (snd image_pair)) ^ " )"

let string_rep_image_min (image_pair: image) =
  "(" ^ string_rep_min (fst image_pair) ^ "," ^ string_of_int (snd image_pair) ^ ")"

let string_rep_event_list (e_list: string list list) =
  let rec string_rep_event_list_rec (e_list: string list list) =
    match e_list with
        hd::[] -> stringrep_event hd
      | hd::tl -> stringrep_event hd ^ ", " ^ string_rep_event_list_rec tl 
      | _ -> ""

  in "{" ^ string_rep_event_list_rec e_list ^ "}" 

let print_tcl (partial_event: d_event) (pair: (ltl * int)) =
  print_endline ((stringrep_devent partial_event) ^ " -> " ^ (string_rep_image pair))

(* ------- *)
(* Oracles *)

let missing_ap (formula: ltl) =  
  let rec ap_formula (formula: ltl) = 
    match formula with 
        True | False | Next _ -> []
      | Var x -> [x]
      | Neg f | Glob f | Ev f -> ap_formula f
      | Or(f1, f2) | And(f1, f2) | Until (f1, f2) | Wuntil (f1, f2) -> ap_formula f1 @ ap_formula f2
      (** Release is defined using Weak until **)
      | _ -> raise (Enforcement_Input_Error(formula))

  in let ap_present = ap_formula formula in

  let rec missing_ap_rec (ap_full: string list) =
    match ap_full with
        hd::tl -> (
          let res = missing_ap_rec tl in
          if List.mem hd ap_present || List.mem hd res then 
            res 
          else 
            hd::(missing_ap_rec tl)
        ) 
      | _ -> []

  in let rec ap_full_formula (formula: ltl) =
    match formula with
        True | False -> []
      | Var x -> [x]
      | Neg f | Glob f | Ev f | Next f -> ap_full_formula f
      | Or(f1, f2) | And(f1, f2) | Until (f1, f2) | Wuntil (f1, f2) -> ap_full_formula f1 @ ap_full_formula f2
      | _ -> raise (Enforcement_Input_Error(formula))
    
  in  missing_ap_rec (ap_full_formula formula)

(* More expansive version of the local oracle *)
let update_alternatives (irr_ap: string list) (corrected_e: d_event list) (alpha: d_alphabet) =

  let rec update_alternative (ap: string) (e: d_event) (alpha: d_alphabet) =
    match e, alpha with
        hd_e::tl_e, hd_a::tl_a -> 
          if List.mem ap hd_a then (
            if List.mem ap hd_e then
              []
            else
              (ap::hd_e)::tl_e
          ) else hd_e::(update_alternative ap tl_e tl_a)
      (* Should never happen as ap is in alpha *)
      | _ -> []

  in let rec update_alternatives_rec (ap: string) (corrected_e: d_event list) =
    match corrected_e with
        hd::tl -> ([update_alternative ap hd alpha]) @ (update_alternatives_rec ap tl)
      | _ -> []

  in let rec iterate_irrelevant_ap (irr_ap: string list) =
    match irr_ap with
        hd::tl -> (update_alternatives_rec hd corrected_e) @ (iterate_irrelevant_ap tl)
      | _ -> []
    
  in corrected_e @ (iterate_irrelevant_ap irr_ap)



let rec is_devent_included (e: d_event) (tr: d_event list) =
  match tr with
      hd::tl -> if test_devent_equality e hd then true else is_devent_included e tl
    | _ -> false

(* d_trace = d_event list *)
let rec is_local_trace_valid (local_tr: d_trace) (corrected_events: d_event list list) =
  match local_tr, corrected_events with
      hd_l::tl_l, hd_e::tl_e -> (
        (* If the current head of corrected_event is empty, then it means we did not need enforcement for this event (no violation)
           so there is no need to check *)
        if hd_e <> [] then
          if is_devent_included hd_l hd_e then is_local_trace_valid tl_l tl_e else false
        else
          is_local_trace_valid tl_l tl_e  
        )
    | _ -> true

(* Oracles end *)
(* ----------- *)

(* The size of the tcl is the size of its encoding as a string. The generated string could be parsed non-ambiguously. *)
let rec get_tcl_size_rec (t_bindings : (d_event * image) list) =
  match t_bindings with
      (e, i)::[] -> (String.length (stringrep_devent e)) + (String.length (string_rep_image_min i))
    | (e, i)::tl -> (String.length (stringrep_devent e)) + (String.length (string_rep_image_min i)) + get_tcl_size_rec tl
    | _ -> 0

let get_tcl_size (t: tcl) = 
  get_tcl_size_rec (Tcl.bindings t)

let nb_update = ref 0
let tcl_size = ref 0
let init_enforcer = ref true
let tot_msg_size = ref 0
let max_tcl_size = ref 0


(* Updates the state of the enforcer, sys_event is the part of the system event that is local to the current enforcer *)
let update_tcl (t_bindings: (d_event * image) list) (sys_event: event) (alpha: alphabet) (apr: string list) =
  let new_tcl = ref Tcl.empty in
  let init_local_observations = generate_local_observations apr in
  (* See the comments in d_enforcement *)
  let local_observations = add_unmodified_ap sys_event apr init_local_observations in

  (* Size of the received tcl *)
  tcl_size := get_tcl_size_rec t_bindings;
  max_tcl_size := max !tcl_size !max_tcl_size;
  if (init_local_observations <> [[]]) then
    (
      if !init_enforcer then (
        init_enforcer := false;
      ) else (
        tot_msg_size := !tot_msg_size + !tcl_size;
        nb_update := !nb_update + 1;
      )
    );

  (* Updates an image once with each local observations *)
  let rec update_image (old_event: d_event) (f: ltl) (n: int) (loc_observ: event list) =
    match loc_observ with
        hd::tl -> (
          let updated_formula = simp (rwi f alpha hd) in
          (* If an event is associated to an empty list of top, it means that this event leads to a violation so we
            remove it from the tcl *)
          (if (simp updated_formula) <> False then 
            new_tcl := Tcl.add (old_event @ [hd]) (updated_formula, n + distance hd sys_event apr) !new_tcl);
          (update_image old_event f n tl)
        )
      | _ -> ()
  
  (* Updates the state of the enforcer (tcl) *)
  in let rec update_tcl_rec (t_bindings: (d_event * image) list) =
    match t_bindings with
        hd::tl -> (
          update_image (fst hd) (fst (snd hd)) (snd (snd hd)) local_observations;
          update_tcl_rec tl
        )
      | _ -> ()

  in update_tcl_rec t_bindings; 
  (* Size of the updated tcl *)
  max_tcl_size := max (get_tcl_size !new_tcl) !max_tcl_size;
  !new_tcl

let final_update (t_bindings: (d_event * image) list) =
  let new_tcl = ref Tcl.empty in
  
  (* Updates the state of the enforcer (tcl) *)
  let rec final_update_rec (t_bindings: (d_event * image) list) =
    match t_bindings with
        hd::tl -> (
          let updated_formula = simp (fst (snd hd)) in
          new_tcl := Tcl.add (fst hd) (updated_formula, (snd (snd hd))) !new_tcl;
          final_update_rec tl
        )
      | _ -> ()

  in final_update_rec t_bindings;
  !new_tcl


(* Builds a list the best images w.r.t. to their distance to the system event *)
let rec get_candidates (t_bindings: (d_event * image) list) =
  match t_bindings with
      hd::[] -> [hd]
    | hd::tl -> (
        let candidates = get_candidates (tl) in
        let n = snd (snd hd) in
        let best_candidate = List.hd candidates in
        let n_candidate = snd (snd best_candidate) in
        (* If the distance is worse then we skip this event *)
        if n > n_candidate then 
          candidates
        else ( 
          (* If this event has a better distance, we remove the previous candidates *)
          if n < n_candidate then
            [hd]
          (* If the distance is the same, then we look at the size of the formula to try to pick one over the other *)
          else (
            let formula_candidate_length = String.length (string_rep_min (fst (snd best_candidate))) in
            let formula_length = String.length (string_rep_min (fst (snd hd))) in
            if formula_candidate_length < formula_length then
              candidates
            else if formula_candidate_length > formula_length then
              [hd]
            else
              hd::candidates
          )
        )
      )
    (* Should never happen, this would mean that the original formula is equivalent to False (no candidate at the end of the evaluation) *)
    | _ -> []

(* All the elements in candidates are considered equivalent so we pick the first one based on the lexicographic order of
  their event *)
let rec pick_candidate (candidates: (d_event * image) list) =
  match candidates with
      hd::[] -> hd
    | hd::tl -> (
      let current_best = pick_candidate tl in
      let current_best_event = fst current_best in
      let hd_event = fst hd in
      (* The result cannot be equal to 0, otherwise it would mean that there is two occurences of an event in the tcl
        which should not be possible *)
      if compare_event (List.concat current_best_event) (List.concat hd_event) > 0 then
        hd
      else
        current_best
    )
    (* Cannot happen as there should be more than one candidate here *)
    | _ -> raise Enforcement_Error

(* Function containing the enforcement of a single event. It returns the potentially 
  modified event as well as the next formula to enforce. Iterates over the monitors.
  This is the implementation of the version based on global exploration *)
let rec d_enforce_event_global (t: tcl) (e: d_event) (d_alpha: d_alphabet) =
  let t_bindings = Tcl.bindings t in
  let t_image = List.map (fun (_, i) -> i) t_bindings in

  (* Need to check what happens with an "empty" local event *)
  (* (e and d_alpha necessarily have the same size) *)
  match d_alpha, e with
    (* This case corresponds to the end of the evaluation, i.e. every enforcer has updated
      its state and we can apply the decision *)
      [], [] -> (
        let candidates = get_candidates t_bindings in
        (* If this list is empty, then it means that the formula was unsatisfiable so we return an empty event to signal it *)
        if List.length candidates = 0 then (
          max_tcl_size := 0;
          (Tcl.empty, [], False, 0, max (!nb_update - 1) 0, !tot_msg_size)
        ) else
          let chosen_candidate = pick_candidate candidates in
          (* Returns a tuple for now (for testing purposes) but this will probably not return the tcl in the end *)
          (t, fst chosen_candidate, simp (remove_next (fst (snd chosen_candidate))), snd (snd chosen_candidate), max !nb_update 0, !tot_msg_size)
      )
    (* Evaluation for the "head" monitor in the lists. This does not pick the next enforcer based on what's
      left in the formula so an enforcer that cannot do anything might "receive" the tcl (which shouldn't be
      an issue but this could be improved)
      It is also worth noting that the tcl is not "sent" to the other enforcers here which would be required
      in an actual distributed system *)
    | hd_alpha::tl_alpha, hd_e::tl_e -> (
        let apr = relevant_ap t_image hd_alpha in 
        let updated_tcl = update_tcl t_bindings hd_e hd_alpha apr in
        d_enforce_event_global updated_tcl tl_e tl_alpha
      )
    | _ -> raise Enforcement_Error

(* Function containing the enforcement of a single event. It returns the potentially 
  modified event as well as the next formula to enforce. Iterates over the monitors.
  This is the implementation of the version based on local exploration *)
let rec d_enforce_event_local (t: tcl) (e: d_event) (d_alpha: d_alphabet) =
  let t_bindings = Tcl.bindings t in
  let t_image = List.map (fun (_, i) -> i) t_bindings in

  (* Need to check what happens with an "empty" local event *)
  (* (e and d_alpha necessarily have the same size) *)
  match d_alpha, e with
    (* This case corresponds to the end of the evaluation, i.e. every enforcer has updated
      its state and build the next formula.
      At this point, the tcl should only contain a single entry as the local decision has already been applied *)
      [], [] -> (
        let chosen_candidate = List.hd t_bindings in
        (t, fst chosen_candidate, simp (remove_next (fst (snd chosen_candidate))), snd (snd chosen_candidate), max !nb_update 0, !tot_msg_size)
      )
    (* Evaluation for the "head" monitor in the lists. 
      The decision is applied after the update here as we are in the local exploration version
      This does not pick the next enforcer based on what's left in the formula so an enforcer that cannot do 
      anything might "receive" the tcl (which shouldn't be an issue but this could be improved).
      It is also worth noting that the tcl is not "sent" to the other enforcers here which would be required
      in an actual distributed system. *)
    | hd_alpha::tl_alpha, hd_e::tl_e -> (
        let apr = relevant_ap t_image hd_alpha in 
        let updated_tcl = update_tcl t_bindings hd_e hd_alpha apr in
        let candidates = get_candidates (Tcl.bindings updated_tcl) in
        (* If this list is empty, then it means that the formula was unsatisfiable so we return an empty event to signal it *)
        if List.length candidates = 0 then (
          max_tcl_size := 0; 
          (Tcl.empty, [], False, 0, max (!nb_update - 1) 0, !tot_msg_size)
        ) else
          let chosen_candidate = pick_candidate candidates in 
          let new_tcl = Tcl.empty in
          let new_tcl = Tcl.add (fst chosen_candidate) (snd chosen_candidate) new_tcl in
          d_enforce_event_local new_tcl tl_e tl_alpha
      )
    | _ -> raise Enforcement_Error

let rec d_monitor (f: ltl) (e: d_event) (d_alpha: d_alphabet) =
  match d_alpha, e with
      [], [] -> (
        f
      )
    | hd_alpha::tl_alpha, hd_e::tl_e -> (
        let f_updated = rwi f hd_alpha hd_e in
        if f <> f_updated then (
          if !init_enforcer = true then
            init_enforcer := false
          else (
            nb_update := !nb_update + 1;
            tot_msg_size := !tot_msg_size + String.length (string_rep_simple f_updated)
          )
        );

        let simp_f = simp f_updated in
        if simp_f <> True && simp_f <> False then
          d_monitor simp_f tl_e tl_alpha
        else
          simp_f
      )
    | _ -> raise Enforcement_Error

(* Preliminary transformation done by the first monitor before the evaluation phase *)
let init_transf (f: ltl) =
  let f_rwt = rwT f in
  let t = Tcl.empty in
  let t = Tcl.add [] (f_rwt, 0) t in
  t

(* Main function taking as argument the initial formula, the alphabet over the whole system and
  a trace of event. It returns the enforced trace. This function also takes a flag as a parameter
  that states whether the global or local version should be used*)
let d_enforcer  (formula: ltl) (alpha: d_alphabet) (tr: d_trace) (global: bool) (always_enforce: bool) =
  let toRead = tr in
  max_tcl_size := 0;

  let rec d_enforcer_rec (formula: ltl) (ed_number: int) (toRead: d_trace) =
    let t = init_transf formula in
    match toRead with
        hd::tl -> (
          nb_update := 0; tot_msg_size := 0; tcl_size := 0; init_enforcer := true;
          (* Only verify at first (if the flag is not set) and compute corrections of the event if a violation is detected *)
          let verdict = (
            if not always_enforce then 
              d_monitor (rwT formula) hd alpha 
            else
              False
          ) in
          (* Enforce the current event *)
          let (t, enforced_event, new_formula, nb_modif, nb_msg, msg_size) = (
            if verdict <> False then
              (Tcl.empty, hd, simp (remove_next verdict), 0, !nb_update, !tot_msg_size)
            else (
              init_enforcer := true;
              if global then 
                d_enforce_event_global t hd alpha
              else
                d_enforce_event_local t hd alpha
            )
          ) in
          (* Update the alternatives for the oracles *)
          let t_events = (
            if global && verdict = False then (
              let t_bindings = Tcl.bindings t in
              update_alternatives (missing_ap formula) (List.map (fun (e, _) -> e) t_bindings) alpha
            ) else 
              []
          ) in

          if new_formula <> True && new_formula <> False then
              let (enforced_trace, ed_n, total_nb_modif, total_nb_msg, total_msg_size, max_size, corrected_events) = d_enforcer_rec new_formula (ed_number + 1) tl in
            (enforced_event::enforced_trace, ed_n, nb_modif::total_nb_modif, nb_msg::total_nb_msg, msg_size::total_msg_size, max_size, t_events::corrected_events)
          else
            ([enforced_event], (ed_number + 1), [nb_modif], [nb_msg], [msg_size], !max_tcl_size, [t_events])
        )
      | _ -> ([], ed_number, [], [], [], !max_tcl_size, [])

  in 
  let (enforced_trace, ed_n, nb_modif, nb_msg, msg_size, max_tcl_size, corrected_events) = d_enforcer_rec formula 0 toRead
  in (enforced_trace, ed_n, nb_modif, nb_msg, msg_size, max_tcl_size, corrected_events)

