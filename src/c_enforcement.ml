open Alphabetevent
open Common_enforcement
open List
open Ltl
open Trace
open Utils

module PartialEvent = struct type t = event let compare = compare end
module Tcl = Map.Make(PartialEvent)
type tcl = image Tcl.t

let print_tcl (partial_event: event) (pair: ltl * int) =
  print_endline ((stringrep_event partial_event) ^ " -> " ^ (string_rep_image pair))

let tcl_size (t: tcl) = 
  let rec tcl_size_rec (t_bindings: (event * image) list) =
    match t_bindings with
        (e, i)::tl -> String.length (stringrep_event e) + String.length (string_rep_image_min i) + tcl_size_rec tl
      | _ -> 0
  
  in tcl_size_rec (Tcl.bindings t)

let compute_messages_size (tr: d_trace) =
  let rec compute_messages_size_event (e: d_event) =
    match e with
        hd::tl -> float_of_int (String.length (stringrep_event hd)) +. compute_messages_size_event tl
      | _ -> 0.0

  in let rec compute_messages_size_rec (tr: d_trace) =
    match tr with
        hd::tl -> (compute_messages_size_event hd)::compute_messages_size_rec tl
      | _ -> []
    
  in compute_messages_size_rec tr

let dalpha : d_alphabet ref = ref []

(* Updates the state of the enforcer, sys_event is the part of the system event that is local to the current enforcer *)
let c_update_tcl (t_bindings: (event * image) list) (sys_event: event) (alpha: alphabet) (apr: string list) =
  let new_tcl = ref Tcl.empty in
  let local_observations = add_unmodified_ap sys_event apr (generate_local_observations apr) in

  (* We update the formula using each local event separately instead of the global event so that simplifcations are applied
    at the same point as in the decentralized version.
    Otherwise, simplifying only once after the whole formula has been evaluated with the global event may lead in some
    rare cases to a situation where the formula we get in both versions is different although the event used to evaluate it
    is the same (which is due to the fact that a simplification is applied after the evaluation of each local event in the
    decentralized version)*)
  let rec update_image_local (f: ltl) (e: d_event) (d_alpha: d_alphabet)=
    match e, d_alpha with
        hd_e::tl_e, hd_a::tl_a -> update_image_local (simp (rwi f hd_a hd_e)) tl_e tl_a
      | _ -> f

  (* Updates an image once with each local observations *)
  in let rec update_image (f: ltl) (n: int) (loc_observ: event list) =
    match loc_observ with
        hd::tl -> (
          let d_e = event2devent hd !dalpha in
          let updated_formula = update_image_local f d_e !dalpha in
          (* If an event is associated to an empty list of top, it means that this event leads to a violation so we
            remove it from the tcl *)
          (if updated_formula <> False then
            new_tcl := Tcl.add hd (updated_formula, n + distance hd sys_event apr) !new_tcl);
          (update_image f n tl)
        )
      | _ -> ()
  
  (* Updates the state of the enforcer (tcl) *)
  in let rec update_tcl_rec (t_bindings: (event * image) list) =
    match t_bindings with
        hd::tl -> (
          update_image (fst (snd hd)) (snd (snd hd)) local_observations;
          update_tcl_rec tl
        )
      | _ -> ()

  in update_tcl_rec t_bindings; !new_tcl

(* Builds a list of the best images w.r.t. to their distance to the system event *)
let rec c_get_candidates (t_bindings: (event * image) list) =
  match t_bindings with
      hd::[] -> [hd]
    | hd::tl -> (
        let candidates = c_get_candidates (tl) in
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
  let rec c_pick_candidate (candidates: (event * image) list) =
    match candidates with
        hd::[] -> hd
      | hd::tl -> (
        let current_best = c_pick_candidate tl in
        let current_best_event = fst current_best in
        let hd_event = fst hd in
        (* The result cannot be equal to 0, otherwise it would mean that there is two occurences of an event in the tcl
          which should not be possible *)
        if compare_event current_best_event hd_event > 0 then
          hd
        else
          current_best
      )
      (* Cannot happen as there should be more than one candidate here *)
      | _ -> raise(Enforcement_Error)

let init_transf (f: ltl) =
  let f_rwt = rwT f in
  let t = Tcl.empty in
  let t = Tcl.add [] (f_rwt, 0) t in
  t

let max_tcl_size = ref 0

let c_enforce_event (t: tcl) (e: event) (alpha: alphabet) = 
  let t_bindings = Tcl.bindings t in
  let t_image = List.map (fun (_, i) -> i) t_bindings in
  (* There is one central monitor so the "local" observations talk about the whole system (global) *)
  let apr = relevant_ap t_image alpha in
  let updated_tcl = c_update_tcl t_bindings e alpha apr in
  max_tcl_size := max (tcl_size updated_tcl) !max_tcl_size;
  let candidates = c_get_candidates (Tcl.bindings updated_tcl) in
  (* If this list is empty, then it means that the formula was unsatisfiable so we return an empty event to signal it *)
  if List.length candidates = 0 then
    (Tcl.empty, [], False, 0)
  else
    let chosen_candidate = c_pick_candidate candidates
    in (updated_tcl, fst chosen_candidate, simp (remove_next (fst (snd chosen_candidate))), snd (snd chosen_candidate))
  
let rec c_monitor (f: ltl) (e: d_event) (d_alpha: d_alphabet) =
  match d_alpha, e with
      [], [] -> (
        f
      )
    | hd_alpha::tl_alpha, hd_e::tl_e -> (
        let f_updated = rwi f hd_alpha hd_e in

        (* Monitoring does not add any messages in the centralized setting as the central enforcer already has all the info *)

        let simp_f = simp f_updated in
        if simp_f <> True && simp_f <> False then
          c_monitor simp_f tl_e tl_alpha
        else
          simp_f
      )
    | _ -> raise Enforcement_Error

let c_enforcer (formula: ltl) (d_alpha: d_alphabet) (tr: trace) (always_enforce: bool) =
  dalpha := d_alpha;
  let alpha = List.concat d_alpha in
  max_tcl_size := 0;

  let rec c_enforce_rec (f: ltl) (e_number: int) (toRead: trace) = 
    let t = init_transf f in

    match toRead with
        hd::tl -> (
          let verdict = (
            if not always_enforce then
              c_monitor (rwT f) (event2devent hd !dalpha) !dalpha
            else
              False
          ) in
          let (_, enforced_event, next_formula, nb_modif) = (
            if verdict <> False then
              (Tcl.empty, hd, simp (remove_next verdict), 0)
            else
              c_enforce_event t hd alpha 
          ) in
          if next_formula <> True && next_formula <> False then
            let (enforced_trace, e_n, total_nb_modif, max_size) = c_enforce_rec next_formula (e_number + 1) tl in
            (enforced_event::enforced_trace, e_n, nb_modif::total_nb_modif, max_size)
          else
            ([enforced_event], (e_number + 1), [nb_modif], !max_tcl_size)
        )
      | _ -> ([], e_number, [], !max_tcl_size)
  
  in 
  let (enforced_trace, e_number, nb_modif, max_tcl_size) = c_enforce_rec formula 0 tr in
  (enforced_trace, e_number, nb_modif, max_tcl_size)  