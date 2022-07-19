(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com>, Ylies Falcone <ylies.falcone@gmail.com> 
   Code is released under the GPL license version 3. *)

open Alphabetevent
open Ltl
open C_progression
open C_monitoring
open List
open Utils
open Trace

(* Performs the decentralized progression with a central oberver. This case reduces to the case of centralized monitoring by collapsing the different parts of the event to a single one *)
let d_progress_cent (form:ltl) (e:d_event) =
		cprogress form (List.concat e)

(* Performs the decentralized monitoring with a central oberver. This case reduces to the case of centralized monitoring by collapsing the different parts of the event to a single one *)
let d_monitor_cent (f:ltl) (t:d_trace) (t_read:trace)=
	cmonitor f (concat t)
	
(* Computes the number of messages sent when a central observer is available.
 The number of messages equals the length of the trace needed to reach a verdict times the size of the distributed alphabet (i,e., the number of components in the system)*)
let nb_messages_sent (f:ltl) (t:d_trace) (d_alphabet)=
  let (verdict, witnesslength,_) = d_monitor_cent f t [] in
  (length witnesslength) * length d_alphabet

exception DProgress_Error of ltl*event*d_alphabet*d_trace*int

(* Progresses a formula with an event in a decentralized fashion. This function uses the local observation alphabet of component on which the progression is applied to.
 It also uses:
 	- the global trace (so as to possibly retrieving past events),
 	- the current event read in the trace, and
 	- the component index (to select the needed observation in the current event or a past event)
*) 	

let rec d_progress_form (formula:ltl) (e:event) (alpha:d_alphabet) (t:d_trace) (ed_number:int) (component:int)=
  let form = simp formula in
  let rec d_progress_form_rec (f:ltl) =
    match f with 
	SHARP -> SHARP
      | True -> True
      | False -> False
      | Var v -> if (List.mem v e)
	then True
	else (if (List.mem v (List.nth alpha component))
	  then False
	  else Previous (Var v)
	)
      | Previous (e) -> let alphabeti = List.nth alpha component
      and t_m = List.nth t (ed_number-(number_of_previous form)) in
			let t_m_i = List.nth t_m component in
			(match (retrieve_nested_formula_in_a_previous form) with
			    Var v when ((List.mem v alphabeti)
					&& (List.mem v t_m_i)) -> True
			  | Var v when ((List.mem v alphabeti)) -> False
			  | _ -> Previous (form)
			)
      | Or (f1,f2) -> (Or(d_progress_form_rec f1, d_progress_form_rec f2))
      | And(f1,f2) -> (And(d_progress_form_rec f1 , d_progress_form_rec f2))
      | Neg(f1) -> (Neg (d_progress_form_rec f1))
      | Next(f1) -> f1
      | Until (f1,f2) -> (Or(d_progress_form_rec f2 , And(d_progress_form_rec f1, Until (f1,f2))))
      | Glob (f1) -> (And(d_progress_form_rec f1, Glob f1))
      | Ev(f1) -> (Or (d_progress_form_rec f1, Ev f1))
      | Xor (f1,f2) -> (Xor(d_progress_form_rec f1, d_progress_form_rec f2))
	
      | _ -> raise (DProgress_Error(form,e,alpha,t,ed_number))
	
  in d_progress_form_rec form

(*
 Performs the decentralized progression of an array of formulae. The array of formulae represents the set of obligations currently monitored in the system. 
 This function simply invokes on each formula in the array the decentralized progression that applies to one formula.
*)
let md_progress (formulae:ltl array) (ed:d_event) (alpha:d_alphabet) (t:d_trace) (ed_number:int) =
	for i=0 to (Array.length formulae-1) do
			let edi = List.nth ed i in
			try
			formulae.(i) <- simp (d_progress_form formulae.(i) edi (alpha:d_alphabet) (t:d_trace) (ed_number:int) i)
			with DProgress_Error (form,e,alpha,t,ed_number) -> (
				print_endline("Error when progressing "^string_rep form);
				print_endline("\t with event "^stringrep_event e);
				print_endline("\t and alphabet "^stringrep_alphabet (globalAlphabet alpha));
				)
		done;
		formulae
