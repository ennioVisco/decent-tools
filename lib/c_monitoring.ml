open Alphabetevent
open Trace
open Ltl
open Utils
open C_progression
open Common_monitoring

exception Error

(** Conversion of truth-value to Ltl **)
let to_dltl (v:truth_value) : ltl =
  match v with
    | True -> True
    | False -> False
    | _ -> failwith "This should not happen: truth-value Undetermined cannot be converted to a DLtl formula"

  
let current_time : int ref = ref (-1)
let main_formula : ltl ref = ref Ltl.True
let nb_progressions : int ref = ref 0


let init_monitoring (f:ltl) : unit =
  main_formula := f;
  current_time := -1;
  nb_progressions := 0
  
let end_of_monitoring (tv : truth_value) (t:trace) : truth_value * trace * int =
  monitoring_message ("The final verdict obtained at time "^(string_of_int !current_time)^" is: "^(stringrep_truthvalue tv));
  monitoring_message ("END READING EVENTS");
  monitoring_message ("END C_MONITORING");
  (tv, t, !nb_progressions)

(** Returns the truth-value, the trace needed to reach this
    truth-value, and the number of progressions **)
let cmonitor (phi:ltl) (t:trace) : truth_value * trace * int=
  let make_one_monitoring_step (e:event) : unit =
    current_time := !current_time + 1;
    nb_progressions := !nb_progressions + count_needed_progressions !main_formula;
    main_formula := simp (cprogress !main_formula e);
    monitoring_message ("t = "^string_of_int !current_time^", after reading "^(stringrep_event e)^": "^string_rep !main_formula);
    if (!main_formula = True || !main_formula = False) then
      (
	raise (End_of_Monitoring(!current_time, to_truth_value !main_formula))
      ) in
  monitoring_message ("BEGIN C_MONITORING");
  init_monitoring phi;
  monitoring_message ("BEGIN READING EVENTS");
  monitoring_message ("t = -1: "^string_rep phi);
  try
    List.iter (fun elt -> make_one_monitoring_step elt) t;
    end_of_monitoring Undeter (prefix (!current_time) t)
  with
    | End_of_Monitoring (time, tv) -> end_of_monitoring tv (prefix time t)

(**

let monitor (f:ltl) (t:trace) = 
  let the_length = List.length t in
  let rec monitor_rec (f:ltl) (current:int) =	
    match (simp f) with
	True -> (True,if current=0 then [] else (if current > (the_length-1) then t else prefix current t))
      | False -> (False, if current=0 then [] else (if current > (the_length - 1) then t else prefix current t))
      | _ -> (
	if (current = the_length-1) 
	then match (progress f (List.nth t current)) with
	    True -> (True,t)
	  | _ -> (False,t)
	else
	  (
	    if (current< the_length-1) then match (progress f (List.nth t current)) with
	      | True -> (True, (prefix (current+1)t))
	      | False -> (False, (prefix (current+1) t))
	      | obligation ->  monitor_rec obligation (current+1)
	    else raise Error
	  )
      )
  in monitor_rec f  0
**)
