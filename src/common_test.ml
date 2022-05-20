open Utils
open List
open Alphabetevent
open Trace
open Architecture
open Ltl
open Dltl
open Ltl_generator
open Network
open Printers
open Common_network
open Dyn_network
open C_monitoring
open Od_monitoring
open Md_monitoring
open Common_enforcement
open D_enforcement
open C_enforcement



(** ///////////////////////////////////////////////////////////
    Utils
    ///////////////////////////////////////////////////////////**)

let test_verbose : bool ref = ref true
let error_verbose : bool ref = ref true

let separator_line = "///********************************************///"
  
let print_separator_line () : unit =
  print_endline (separator_line)

let test_message (msg:string) : unit =
  if (!test_verbose) then
    print_endline (msg)
      
let error_message (msg:string) : unit =
  if (!error_verbose) then (
    print_separator_line ();
    print_endline ("ERROR: "^msg);
    print_separator_line ();
  )

(**
   ///////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////
   Tests related to static
   networks
   ///////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////**)
    
(** ///////////////////////////////////////////////////////////
    Test the creation of a static network
    ///////////////////////////////////////////////////////////**)

let test_network (archi: (string list) Architecture.t) (phi:dltl) : network =
  print_separator_line ();
  test_message ("Considered architecture:\n"^stringrep_architecture archi);
  test_message ("Considered formula: "^string_rep phi);
  let chc_phi = chc phi archi 0 in
  test_message ("Chosen initial component: "^string_of_int chc_phi);
  let empty_network = make_empty_network (Architecture.cardinal archi) in
  let newnet = network archi chc_phi empty_network phi in
  test_message ("Resulting network:\n"^stringrep_network newnet);
  newnet

(** //////////////////////////////////////////////////////////////
    Test the computation of compacting the network
    ///////////////////////////////////////////////////////////**)
    
let test_compact (n:network) : network =
  print_separator_line ();
  test_message ("Input network (before compacting):\n"^stringrep_network n);
  let compactnet = compact n in
  test_message ("Resulting network (after compacting):\n"^stringrep_network compactnet);
  compactnet

(** //////////////////////////////////////////////////////////////
    Test the reindexation of the cell of the network after a
    compacting.
    ///////////////////////////////////////////////////////////**)
    
let test_reindex (n:network) : network =
  print_separator_line ();
  test_message ("Initial network (before reindexing):\n"^stringrep_network n);
  let reindexnet = reindex n in
  test_message ("Resulting network (after reindexing):\n"^stringrep_network reindexnet);
  reindexnet

(** //////////////////////////////////////////////////////////////
    Test the computation of automatically respawning cells in a static
    network
    ///////////////////////////////////////////////////////////**)
let test_respawn (n:network) (archi:architecture) (phi:dltl) : coordinates list =
  print_separator_line ();
  test_message ("Computing automatically respawning cells for the following network:");
  test_message (stringrep_network n);
  let respawning = compute_respawn archi n phi in
  let srep_respawning = stringrep_intint_list respawning in
  test_message ("Automatically respawning cells:");
  test_message (srep_respawning);
  respawning

(** //////////////////////////////////////////////////////////////
    Test the computation of referents of a network
    ///////////////////////////////////////////////////////////**)
let test_referents (n:network) : referents_network =
  print_separator_line ();
  test_message ("Computing referents for the following network:");
  test_message (stringrep_network n);
  let refs = referents_of_network n in
  test_message ("Referents are:");  
  test_message (stringrep_referents_network refs);
  refs

(** //////////////////////////////////////////////////////////////
    Test the computation of referents of a network
    ///////////////////////////////////////////////////////////**)
let test_referrers (n:network) : referrers_network =
  print_separator_line ();
  test_message ("Computing referrers for the following network:");
  test_message (stringrep_network n);
  let refs = referrers_of_network n in
  test_message ("Referrers are:");  
  test_message (stringrep_referrers_network refs);
  refs
    
(** ///////////////////////////////////////////////////////////
    Launching all tests all at once
    ///////////////////////////////////////////////////////////**)

let test_static_network (phi:dltl) (dalpha:d_alphabet) : network =
  let archi = dalphabet_2_architecture dalpha in
  let net = test_network archi phi in
  let net_compact = test_compact net in
  let net_reindex = test_reindex net_compact in
  let _ = test_respawn net_reindex archi phi in
  let _ = test_referents net_reindex in
  let _ = test_referrers net_reindex in
  net_reindex
    
    
(** //////////////////////////////////////////////////////////////
    Test the conversion from a static network to a dynamic network
    ///////////////////////////////////////////////////////////**)

let test_conversion (n:network) : dyn_net =
  print_separator_line ();
  test_message ("Converting the following static network");
  test_message (stringrep_network n);
  test_message ("Into the following dynamic network");
  let dynnet = make_dyn_net n in
  test_message (stringrep_dyn_net dynnet);
  dynnet


(** /////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////
    Tests related to orchestration based-monitoring (compared to centralised monitoring)
    ///////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////**)

(** //////////////////////////////////////////////////////////////
    Test the decentralised monitoring of a trace against a
    property.  Trace and property are passed as parameters.
    ///////////////////////////////////////////////////////////**)

let test_dmonitoring_1trace_1formula (dalpha:d_alphabet) (phi:dltl) (t:trace) : unit =
  let archi = dalphabet_2_architecture dalpha in
  print_separator_line ();
  test_message ("Considered architecture:\n"^stringrep_architecture archi);
  test_message ("Considered formula: "^string_rep phi);
  test_message ("Considered trace: "^stringrep_trace t);
  test_message ("Launching monitoring.");
  let _ = od_monitor archi phi t in ()

(** //////////////////////////////////////////////////////////////
    Test the decentralised monitoring of a trace against a property.
    Trace and property are randomly generated.
    ///////////////////////////////////////////////////////////**)
let test_dmonitoring_1randomTrace_1randomFormula (dalpha:d_alphabet) (size_form:int) (size_trace:int) =
  let alpha = globalAlphabet dalpha in
  let _ = test_dmonitoring_1trace_1formula dalpha (ltl_2_dltl (gen_1_form_nontrivial size_form alpha)) (gen_1_trace size_trace alpha)
  in ()

(** /////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////
    Functional test of the monitoring algorithm
    ///////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////**)

let test_correctness_monitoring_algo (dalpha:d_alphabet) (phi:dltl) (t:trace) : unit =
  let archi = dalphabet_2_architecture dalpha and
      alpha = globalAlphabet dalpha in
  print_separator_line ();
  test_message ("Considered architecture:\n"^stringrep_architecture archi);
  test_message ("Considered formula: "^string_rep phi);
  test_message ("Considered trace: "^stringrep_trace t);
  test_message ("Launching monitoring.");
  let vdmon = od_monitor archi phi t and (vcmon,_,_) = cmonitor (dltl_2_ltl phi) t in
  if (vdmon <> vcmon) then
    error_message ("Different verdicts when monitoring when formula "^string_rep phi^" on trace "^stringrep_trace t)

let test_correctness_monitoring_algo_1randomTrace_1Formula (dalpha:d_alphabet) (phi:dltl) (size_trace:int) =
  let phi = simp phi in
  let alpha = globalAlphabet dalpha in
  let _ = test_correctness_monitoring_algo dalpha phi (gen_1_trace size_trace alpha)
  in ()

let test_correctness_monitoring_algo_XrandomTraces_1Formula (dalpha:d_alphabet) (phi:dltl) (nb_traces:int) (size_trace:int) =
  let phi = simp phi in
  let alpha = globalAlphabet dalpha in
  for cpt = 1 to nb_traces do
  test_correctness_monitoring_algo dalpha phi (gen_1_trace size_trace alpha)
  done

let test_correctness_monitoring_algo_1randomTrace_1randomFormula (dalpha:d_alphabet) (size_form:int) (size_trace:int) =
  let alpha = globalAlphabet dalpha in
  let _ = test_correctness_monitoring_algo dalpha (ltl_2_dltl (gen_1_form_nontrivial size_form alpha)) (gen_1_trace size_trace alpha)
  in ()

let test_correctness_monitoring_algo_X_randomTraces_randomFormulae (nbTests:int) (dalpha:d_alphabet) (size_form:int) (size_trace:int) =
  for cpt = 1 to nbTests do
    test_correctness_monitoring_algo_1randomTrace_1randomFormula dalpha size_form size_trace
  done


    (** /////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////
    Comparion tests related to orchestration-based vs migration-based
    monitoring
    ///////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////**)

let rec how_many_props_differ (e1:event) (e2:event) (alpha:alphabet):int =
  match alpha with
      [] -> 0
    | p::tail -> (if ((List.mem p e1 && List.mem p e2) || not (List.mem p e1 || List.mem p e2)) then 0 else 1) + how_many_props_differ e1 e2 tail

let number_ofupdown_fronts (t:d_trace) (dalpha:d_alphabet) = 
  let rec number_of_updwon_fronts_rec (t:d_trace) (dalpha:d_alphabet)  (acc:int) =
    match t with
	[] -> acc
      | elt::[] -> acc
      | e1::e2::remainder -> 
	let fronts_current_event = List.fold_left (fun x y -> x + (if y>0 then 1 else 0)) 0 (map3 how_many_props_differ e1 e2 dalpha) in
	number_of_updwon_fronts_rec (e2::remainder) dalpha (fronts_current_event+acc)
  in number_of_updwon_fronts_rec t dalpha (List.length dalpha)

  
type message_passing = SEND_EVERYTHING | SEND_CHANGES

let mode  = ref SEND_EVERYTHING 

let number_of_messages_centralized_case (traceNeededCent:trace) (traceNeededDecent:d_trace) (dalpha:d_alphabet) =

  match !mode with
    | SEND_EVERYTHING -> (List.length traceNeededCent) * (List.length dalpha)
    | SEND_CHANGES -> 
      let the_prefix = (prefix ((length traceNeededCent)) traceNeededDecent) in 
      let tmp = number_ofupdown_fronts the_prefix dalpha in
      (*
	print_endline (if the_prefix = [] then "empty" else (string_rep_dtrace the_prefix));
	print_int tmp; 
      *)
      tmp (* a bit tricky here. We want the prefix of the d_trace of length of the (c_)trace; because the function number_of_updown_fronts uses the d_trace directly and we want to know what would be the number of messages if there was a central observation point that collects the observation coming from the d_tracec *)
		

let size_of_messages_cent (traceNeededCent:trace) (dalpha:d_alphabet) =
  float_of_int (length (globalAlphabet dalpha)) /. float_of_int (length dalpha)

(*
Compares centralized and decentralized monitoring in terms of trace needed to get a verdict and number of messages exchanged
*)
let compare_results (formula:ltl) (alpha:d_alphabet) (trace:d_trace) =
  
  let size = List.length alpha in
  (
    let global_trace = globalTrace trace and
	array_of_form = Array.make size formula and
	init_oblig = Array.make size (True:ltl) in
    let (verdict_centralized,traceTmp,nb_progressions_cent) = cmonitor formula global_trace and
	(verdict_Mdecent,traceTmpMDecent, num_mess_Mdecent, size_mess_Mdecent, nb_progressions_decent) = md_monitor array_of_form alpha trace 0 init_oblig trace [] 0 0. and
	(verdict_Odecent, traceTmpODecent, num_mess_Odecent, size_mess_Odecent, nb_progressions_odecent) = od_monitor_stats (dalphabet_2_architecture alpha) (ltl_2_dltl formula) (globalTrace trace) in
    (
      let num_mess_cent = number_of_messages_centralized_case traceTmp traceTmpMDecent alpha in
      let size_mess_cent = size_of_messages_cent traceTmp alpha in
      (*the total number of exchanged messages is the size of the d_alphabet * the length of the trace*)
      (**
	 if (List.length traceTmp > List.length traceTmpMDecent) then
	 (
	 print_endline ("Error for formula :"^Ltl.string_rep formula);
	 print_endline ("initial dtrace : "^stringrep_dtrace trace);
	 print_endline ("trace : "^stringrep_trace traceTmp);
	 print_endline ("dtrace : "^stringrep_dtrace traceTmpMDecent);
	 );
      **)
      (formula, trace, verdict_centralized, verdict_Mdecent, verdict_Odecent, traceTmp, traceTmpMDecent, traceTmpODecent, num_mess_cent, num_mess_Mdecent, num_mess_Odecent, size_mess_cent, size_mess_Mdecent, size_mess_Odecent, nb_progressions_cent, nb_progressions_decent, nb_progressions_odecent)
    )
  )

let rec generate_compared_results_efficient (alpha:d_alphabet) (size_form:int) (size_trace:int) (bias:bool) =
  let trace = gen_1_dtrace size_trace alpha
  and formula : ltl =
    if (bias) then
      Ltl.simp (Ltl_generator.gen_1_form_nontrivial_biased size_form alpha (Random.int (length alpha)))
    else
      Ltl.simp (Ltl_generator.gen_1_form_nontrivial size_form (globalAlphabet alpha))
  in
  let (formula, trace,
       verdict_cent, verdict_Mdecent, verdict_Odecent,
       traceNeededCent,traceNeededMDeCent,traceNeededODecent,
       ncent, nMDecent, nODecent,
       smesscent, smessdecent, smessodecent,
  nbProgCent, nbProgDecent, nbProgODecent) = 
    compare_results formula alpha trace in
  let meaningful = verdict_cent = verdict_Mdecent && verdict_cent = verdict_Odecent && List.length traceNeededCent < size_trace && List.length traceNeededMDeCent < size_trace && List.length traceNeededODecent < size_trace && List.length traceNeededCent > 3* (size_form -1) in
  (meaningful, List.length traceNeededCent + 1 , List.length traceNeededMDeCent + 1, List.length traceNeededODecent + 1,ncent,nMDecent,nODecent,smesscent,smessdecent,smessodecent, nbProgCent, nbProgDecent, nbProgODecent)


let generate_compared_results_efficient_patterns (alpha:d_alphabet) (kind:string) (size_trace:int)  =
  let trace = gen_1_dtrace size_trace alpha
  and formula :ltl =
    if (kind="abs") then Ltl.simp (gen_1_form_abscence (globalAlphabet alpha))
    else if (kind="exist") then Ltl.simp (gen_1_form_existence (globalAlphabet alpha))
    else if (kind="unive") then Ltl.simp (gen_1_form_universality (globalAlphabet alpha))
    else if (kind="bexist") then Ltl.simp (gen_1_form_boundedexistence (globalAlphabet alpha))
    else if (kind="prec") then Ltl.simp (gen_1_form_precedence (globalAlphabet alpha))
    else if (kind="resp") then Ltl.simp (gen_1_form_response (globalAlphabet alpha))
    else if (kind="pchain") then Ltl.simp (gen_1_form_precedence_chain (globalAlphabet alpha))
    else if (kind="rchain") then Ltl.simp (gen_1_form_response_chain (globalAlphabet alpha))
    else if (kind="cchain") then Ltl.simp (gen_1_form_constrained_chain (globalAlphabet alpha))
    else Ltl.simp (gen_1_form_abscence (globalAlphabet alpha)) in
  (* print_endline(string_rep_input(formula)); *)
  let (formula, trace,
       verdict_cent, verdict_Mdecent, verdict_Odecent,
       traceNeededCent,traceNeededMDeCent,traceNeededODecent,
       ncent, nMDecent, nODecent,
       smesscent, smessdecent, smessodecent,
  nbProgCent, nbProgDecent, nbProgODecent) = 
    compare_results formula alpha trace in
  let meaningful = verdict_cent = verdict_Mdecent && verdict_cent = verdict_Odecent && List.length traceNeededCent < size_trace && List.length traceNeededMDeCent < size_trace && List.length traceNeededODecent < size_trace && List.length traceNeededCent > 10 in
  (meaningful, List.length traceNeededCent + 1, List.length traceNeededMDeCent + 1, List.length traceNeededODecent + 1,ncent,nMDecent,nODecent,smesscent,smessdecent,smessodecent, nbProgCent, nbProgDecent, nbProgODecent)


(**
   /////////////////////////////////////////////////////////////
   /////////////////////////////////////////////////////////////
   Assessing the effect of 
   ///////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////**)


let create_static_network (phi:dltl) (archi: architecture) : network =
  let chc_phi = chc phi archi 0 in
  let empty_network = make_empty_network (Architecture.cardinal archi) in
  let net = network archi chc_phi empty_network phi in
  let compactnet = compact net in
  reindex compactnet

let depth_with_one_dalpha (dalpha:d_alphabet) (form:dltl) : int =
  let archi = dalphabet_2_architecture dalpha in
  let net = create_static_network form archi in
  depth_network net archi form

let has_all_depths (alpha_size:int) (form:dltl) (dalphas:d_alphabet list) : bool =
  let has_given_depth (d:int) : bool =
    List.exists (fun elt -> depth_with_one_dalpha elt form = d) dalphas in
  let ok = ref true in
  for depth = 1 to alpha_size do
    ok := !ok && has_given_depth depth
  done;
  !ok
  
let compute_one_result (dalpha:d_alphabet) (trace:trace) (formula:ltl) =
  let size_trace = List.length trace in
  let dtrace = trace2dtrace trace dalpha in
  let formulad = ltl_2_dltl formula in
  let (formula, trace,
       verdict_cent, verdict_Mdecent, verdict_Odecent,
       traceNeededCent,traceNeededMDeCent,traceNeededODecent,
       ncent, nMDecent, nODecent,
       smesscent, smessdecent, smessodecent,
       nbProgCent, nbProgDecent, nbProgODecent) = 
    compare_results formula dalpha dtrace in
  let meaningful = verdict_cent = verdict_Mdecent && verdict_cent = verdict_Odecent && List.length traceNeededCent < size_trace && List.length traceNeededMDeCent < size_trace && List.length traceNeededODecent < size_trace in
  (meaningful, List.length dalpha, depth_with_one_dalpha dalpha formulad, List.length traceNeededCent, List.length traceNeededMDeCent, List.length traceNeededODecent, ncent,nMDecent, nODecent, smesscent, smessdecent, smessodecent, nbProgCent, nbProgDecent, nbProgODecent)

let make_one_eval_test (alpha:alphabet) (size_form:int) (size_trace:int) =
  let dalphabets = generate_all_compatible_dalphabets alpha in
  print_endline (stringrep_list stringrep_dalphabet dalphabets);
  print_endline (string_of_int (List.length dalphabets));

  let rec make_one_eval_test_rec () = 
    let formula = Ltl.simp (Ltl_generator.gen_1_form_nontrivial size_form alpha) in
    let formulad = ltl_2_dltl formula in
    let trace = gen_1_trace size_trace alpha in
    if has_all_depths (length alpha) formulad dalphabets then
      List.fold_left
        (fun acc elt -> (compute_one_result elt trace formula)::acc)
        [] dalphabets  
    else
      make_one_eval_test_rec ()

  in make_one_eval_test_rec ()

(* 
Comparison between the different enforcement versions (local/global decentralized and centralized)
*)

(* Function performing a complete round of enforcement and displaying information about the evaluation part *)
let d_enforce_event_info (t: D_enforcement.tcl) (e: d_event) (alpha: d_alphabet) =
  print_endline "\n=======================================================";
  print_endline ("\n\t Enforcement of the event " ^ stringrep_devent e);
  print_endline "\nComplete round of enforcement (GLOBAL) :";
  let (updated_t_global, emitted_event_global, next_formula_global, nb_modif, nb_msg, msg_size) = d_enforce_event_global t e alpha in
  D_enforcement.Tcl.iter D_enforcement.print_tcl updated_t_global;
  print_endline ("Emitted event: " ^ stringrep_devent emitted_event_global ^ " = " ^ stringrep_event (List.concat emitted_event_global)
  ^ " -> " ^ string_of_int nb_modif ^ " modifications");
  let avg_msg_size = (float_of_int msg_size) /. (float_of_int nb_msg) in
  print_endline ("Number of messages: " ^ (string_of_int nb_msg) ^ ", average size of the messages: " ^ (string_of_float avg_msg_size));
  print_endline ("Next formula to enforce: " ^ string_rep_simple next_formula_global);
  print_endline "\nComplete round of enforcement (LOCAL) :";
  let (updated_t_local, emitted_event_local, next_formula_local, nb_modif, nb_msg, msg_size) = d_enforce_event_local t e alpha in
  D_enforcement.Tcl.iter D_enforcement.print_tcl updated_t_local;
  print_endline ("Emitted event: " ^ stringrep_devent emitted_event_local ^ " = " ^ stringrep_event (List.concat emitted_event_local) 
  ^ " -> " ^ string_of_int nb_modif ^ " modifications");
  let avg_msg_size = (float_of_int msg_size) /. (float_of_int nb_msg) in
  print_endline ("Number of messages: " ^ (string_of_int nb_msg) ^ ", average size of the messages: " ^ (string_of_float avg_msg_size));
  print_endline ("Next formula to enforce: " ^ string_rep_simple next_formula_local);
  print_endline (string_rep_tree next_formula_local);
  print_endline "\n=======================================================";
  (updated_t_global, emitted_event_global, next_formula_global, updated_t_local, emitted_event_local, next_formula_local)

(* Function performing a complete round of enforcement on formula f with event e and on alphabet alpha.
  It displays the transformation of the formula and the emitted event at the end. *)
let d_enforce_event_info_full (f: ltl) (e: d_event) (alpha: d_alphabet) =
  print_endline "Original formula:";
  print_formula f;
  print_endline "Rewriting of temporal operators:";
  let f_rewritten = rwT f in
  print_formula f_rewritten;
  let t = D_enforcement.Tcl.empty in
  let t = D_enforcement.Tcl.add [] (f_rewritten, 0) t in
  print_endline "Initial state of the TCL:";
  D_enforcement.Tcl.iter D_enforcement.print_tcl t;
  d_enforce_event_info t e alpha

(* Function performing a complete round of enforcement and displaying information about the evaluation part *)
let c_enforce_event_info (t: C_enforcement.tcl) (e: event) (alpha: alphabet) =
  print_endline "\n=======================================================";
  print_endline ("\n\t Enforcement of the event " ^ stringrep_event e);
  print_endline "\nComplete round of enforcement :";
  let (updated_t, emitted_event, next_formula, _) = c_enforce_event t e alpha in
  C_enforcement.Tcl.iter C_enforcement.print_tcl updated_t;
  print_endline ("Emitted event: " ^ stringrep_event emitted_event);
  print_endline ("Next formula to enforce: " ^ string_rep_simple next_formula);
  print_endline "\n=======================================================";
  (updated_t, emitted_event, next_formula)

let prt_full = ref false
let precision = ref 0
let file_name = ref ""
let stat_file_name = ref ""
let sample_header = ref ""
let produce_tex = ref false
let always_enforce = ref true

let avg_modif_g_list : float list ref = ref []
let avg_nb_msg_g_list : float list ref = ref []
let avg_msg_size_g_list : float list ref = ref []
let max_tcl_size_g_list : int list ref = ref []
let tr_len_g_list : int list ref = ref []
let avg_modif_l_list : float list ref = ref []
let avg_nb_msg_l_list : float list ref = ref []
let avg_msg_size_l_list : float list ref = ref []
let max_tcl_size_l_list : int list ref = ref []
let tr_len_l_list : int list ref = ref []
let nb_diff_event_list : int list ref = ref []
let nb_diff_nb_modif_list_d : int list ref = ref []
let avg_modif_cent_list : float list ref = ref []
let avg_nb_msg_cent_list : float list ref = ref []
let avg_msg_size_cent_list : float list ref = ref []
let max_tcl_size_cent_list : int list ref = ref []
let tr_len_cent_list : int list ref = ref []
let nb_diff_event_list_cent_g : int list ref = ref []
let nb_diff_event_list_cent_l : int list ref = ref []
let nb_diff_nb_modif_list_cent_g : int list ref = ref []
let nb_diff_nb_modif_list_cent_l : int list ref = ref []
let nb_diff_traces = ref 0
let nb_invalid_local_tr = ref 0

let test_info_decent (enforced_trace_g: d_trace) (nb_event_g: int) (nb_modif_g: int list) (nb_msg_g: int list) (msg_size_g: int list) (max_tcl_size_g: int)
                      (enforced_trace_l: d_trace) (nb_event_l: int) (nb_modif_l: int list) (nb_msg_l: int list) (msg_size_l: int list) (max_tcl_size_l: int) =
  if !prt_full then (
    print_endline ("##### GLOBAL EXPLORATION #####");
    print_endline ("Enforced trace: " ^ stringrep_dtrace enforced_trace_g)
  );
  let avg_modif_g = (float_of_int (sum_int_list nb_modif_g)) /. (float_of_int nb_event_g) in
  let avg_nb_msg_g = (float_of_int (sum_int_list nb_msg_g)) /. (float_of_int (List.length nb_msg_g)) in
  let avg_msg_size_g = compute_avg_msg_size msg_size_g nb_msg_g in
  let tr_len_g = List.length enforced_trace_g in
  avg_modif_g_list := avg_modif_g :: !avg_modif_g_list;
  avg_nb_msg_g_list := avg_nb_msg_g :: !avg_nb_msg_g_list;
  avg_msg_size_g_list := avg_msg_size_g :: !avg_msg_size_g_list;
  max_tcl_size_g_list := max_tcl_size_g :: !max_tcl_size_g_list;
  tr_len_g_list := tr_len_g :: !tr_len_g_list;
  if !prt_full then (
    print_endline ("Average number of messages: " ^ string_of_float avg_nb_msg_g);
    print_endline ("Length of the trace: " ^ string_of_int tr_len_g);
    print_endline ("Average size of the messages: " ^ string_of_float avg_msg_size_g);
    print_endline ("Average number of modifications: " ^ string_of_float avg_modif_g);
    print_endline ("Maximum size of the tcl: " ^ string_of_int max_tcl_size_g);

    print_endline ("\n##### LOCAL EXPLORATION #####");
    print_endline ("Enforced trace: " ^ stringrep_dtrace enforced_trace_l)
  );
  let avg_modif_l = (float_of_int (sum_int_list nb_modif_l)) /. (float_of_int nb_event_l) in
  let avg_nb_msg_l = (float_of_int (sum_int_list nb_msg_l)) /. (float_of_int (List.length nb_msg_l)) in
  let avg_msg_size_l = compute_avg_msg_size msg_size_l nb_msg_l in
  let tr_len_l = List.length enforced_trace_l in
  avg_modif_l_list := avg_modif_l :: !avg_modif_l_list;
  avg_nb_msg_l_list := avg_nb_msg_l :: !avg_nb_msg_l_list;
  avg_msg_size_l_list := avg_msg_size_l :: !avg_msg_size_l_list;
  max_tcl_size_l_list := max_tcl_size_l :: !max_tcl_size_l_list;
  tr_len_l_list := tr_len_l :: !tr_len_l_list;
  if !prt_full then (
    print_endline ("Average number of messages: " ^ string_of_float avg_nb_msg_l);
    print_endline ("Lenght of the trace: " ^ string_of_int tr_len_l);
    print_endline ("Average size of the messages: " ^ string_of_float avg_msg_size_l);
    print_endline ("Average number of modifications " ^ string_of_float avg_modif_l);
    print_endline ("Maximum size of the tcl: " ^ string_of_int max_tcl_size_l);
  );

  let nb_diff_event = compare_dtrace enforced_trace_g enforced_trace_l in
  let nb_diff_nb_modif = nb_diff_list nb_modif_g nb_modif_l in
  nb_diff_event_list := nb_diff_event :: !nb_diff_event_list;
  nb_diff_nb_modif_list_d := nb_diff_nb_modif :: !nb_diff_nb_modif_list_d;
  if !prt_full then (
    print_endline ("\nNumber of different event between the two enforced trace: " ^ string_of_int nb_diff_event);
    print_endline ("Number of event with a different number of modifications (compared to the original event) between the two enforced trace: " ^ 
      string_of_int nb_diff_nb_modif)
  )
  

let test_info_cent (alpha: d_alphabet) (tr: d_trace) (enforced_trace: trace) (nb_event: int) (nb_modif_list: int list) (max_tcl_size: int) 
                    (enforced_trace_g: d_trace) (enforced_trace_l: d_trace) (nb_modif_g: int list) (nb_modif_l: int list) =
  let d_enforced_trace = trace2dtrace enforced_trace alpha in
  if !prt_full then (
    print_endline ("Enforced trace: " ^ stringrep_trace enforced_trace);
    print_endline ("Equivalent decentralized trace: " ^ stringrep_dtrace d_enforced_trace)
  );
  let avg_modif = (float_of_int (sum_int_list nb_modif_list)) /. (float_of_int nb_event) in
  (* The average number of messages sent is equal to the number of enforcer as every enforcer sends its observation to the
    central enforcer *)
  let avg_nb_msg = List.length alpha in
  let msg_size = compute_messages_size tr in
  let avg_msg_size = ((sum_float_list msg_size) /. (float_of_int (List.length msg_size))) /. (float_of_int avg_nb_msg) in
  let tr_len = List.length enforced_trace in
  avg_modif_cent_list := avg_modif :: !avg_modif_cent_list;
  avg_nb_msg_cent_list := float_of_int avg_nb_msg :: !avg_nb_msg_cent_list;
  avg_msg_size_cent_list := avg_msg_size :: !avg_msg_size_cent_list;
  max_tcl_size_cent_list := max_tcl_size :: !max_tcl_size_cent_list;
  tr_len_cent_list := tr_len :: !tr_len_cent_list;
  if !prt_full then (
    print_endline ("Average number of messages: " ^ string_of_int avg_nb_msg);
    print_endline ("Length of the trace: " ^ string_of_int tr_len);
    (* Each "external" enforcer sends its local observation to the central enforcer when an event is emitted.
      The messages here should be smaller than in the decentralized case. *)
    print_endline ("Average size of the messages: " ^ string_of_float avg_msg_size);
    print_endline ("Average number of modifications: " ^ string_of_float avg_modif);
    print_endline ("Maximum size of the tcl: " ^ string_of_int max_tcl_size)
  );
  let nb_diff_event_g = compare_dtrace d_enforced_trace enforced_trace_g in
  let nb_diff_event_l = compare_dtrace d_enforced_trace enforced_trace_l in
  nb_diff_event_list_cent_g := nb_diff_event_g :: !nb_diff_event_list_cent_g;
  nb_diff_event_list_cent_l := nb_diff_event_l :: !nb_diff_event_list_cent_l;
  if !prt_full then (
    print_endline "\nNumber of different event between the enforced traces";
    print_endline ("              - Comparing the centralized version with the decentralized global version: " ^ string_of_int nb_diff_event_g);
    print_endline ("              - Comparing the centralized version with the decentralized local version: " ^ string_of_int nb_diff_event_l)
  );
  let nb_diff_nb_modif_g = nb_diff_list nb_modif_list nb_modif_g in
  let nb_diff_nb_modif_l = nb_diff_list nb_modif_list nb_modif_l in
  nb_diff_nb_modif_list_cent_g := nb_diff_nb_modif_g :: !nb_diff_nb_modif_list_cent_g;
  nb_diff_nb_modif_list_cent_l := nb_diff_nb_modif_l :: !nb_diff_nb_modif_list_cent_l;
  if !prt_full then (
    print_endline "\nNumber of event with a different number of modification between the enforced traces";
    print_endline ("              - Comparing the centralized version with the decentralized global version: " ^ string_of_int nb_diff_nb_modif_g);  
    print_endline ("              - Comparing the centralized version with the decentralized local version: " ^ string_of_int nb_diff_nb_modif_l);  

  )

(* Enforcement of trace tr on formula phi (with all versions) *)
let complete_test (phi: ltl) (alpha: d_alphabet) (tr: d_trace) =
  if !prt_full then (
    print_endline ("\nFormula to enforce: ");
    print_formula phi;
    print_endline ("Full trace to enforce: " ^ stringrep_dtrace tr);
    print_endline ("\n\n############## DECENTRALIZED VERSION ################")
  );
  let (enforced_trace_global, nb_event_global, nb_modif_global, nb_msg_global, msg_size_global, max_tcl_size_global, corrected_events) = d_enforcer phi alpha tr true !always_enforce in
  let (enforced_trace_local, nb_event_local, nb_modif_local, nb_msg_local, msg_size_local, max_tcl_size_local, _) = d_enforcer phi alpha tr false !always_enforce in
  test_info_decent enforced_trace_global nb_event_global nb_modif_global nb_msg_global msg_size_global max_tcl_size_global
                    enforced_trace_local nb_event_local nb_modif_local nb_msg_local msg_size_local max_tcl_size_local;

  if !prt_full then
    print_endline ("\n\n############### CENTRALIZED VERSION #################");
  let c_tr = globalTrace tr in
  let (enforced_trace_cent, nb_event_cent, nb_modif_cent, max_tcl_size) = c_enforcer phi alpha c_tr !always_enforce in
  test_info_cent alpha tr enforced_trace_cent nb_event_cent nb_modif_cent max_tcl_size enforced_trace_global enforced_trace_local nb_modif_global nb_modif_local;

  (* Oracles *)
  let enforced_trace_cent_d = trace2dtrace enforced_trace_cent alpha in
  let traces_equal = compare_dtrace enforced_trace_global enforced_trace_cent_d in
  (* Pretty expensive test as we need to check that each event in the decentralized enforced trace is included in the set of alternatives computed in the
      global version. We have to check this independently of the order of the atomic propositions which makes it quite expensive *)
  let local_trace_valid = is_local_trace_valid enforced_trace_local corrected_events in
  if traces_equal <> 0 then nb_diff_traces := !nb_diff_traces + 1;
  if not local_trace_valid then nb_invalid_local_tr := !nb_invalid_local_tr + 1;

  if !file_name <> "" then (
    let space = " " in
    let s_nb_component = string_of_int (List.length alpha) in
    let s_nb_ap = string_of_int (List.length (List.concat alpha)) in
    let s_modif_g = string_of_float (round_generic (List.hd !avg_modif_g_list) !precision) in
    let s_modif_l = string_of_float (round_generic (List.hd !avg_modif_l_list) !precision) in
    let s_modif_cent = string_of_float (round_generic (List.hd !avg_modif_cent_list) !precision) in
    let s_modif = s_modif_g ^ space ^ s_modif_l ^ space ^ s_modif_cent in
    let s_nb_msg_g = string_of_float (round_generic (List.hd !avg_nb_msg_g_list) !precision) in
    let s_nb_msg_l = string_of_float (round_generic (List.hd !avg_nb_msg_l_list) !precision) in
    let s_nb_msg_cent = string_of_float (round_generic (List.hd !avg_nb_msg_cent_list) !precision) in
    let s_nb_msg = s_nb_msg_g ^ space ^ s_nb_msg_l ^ space ^ s_nb_msg_cent in
    let s_msg_size_g = string_of_float (round_generic (List.hd !avg_msg_size_g_list) !precision) in
    let s_msg_size_l = string_of_float (round_generic (List.hd !avg_msg_size_l_list) !precision) in
    let s_msg_size_cent = string_of_float (round_generic (List.hd !avg_msg_size_cent_list) !precision) in
    let s_msg_size = s_msg_size_g ^ space ^ s_msg_size_l ^ space ^ s_msg_size_cent in 
    let s_tcl_size_g = string_of_int (List.hd !max_tcl_size_g_list) in
    let s_tcl_size_l = string_of_int (List.hd !max_tcl_size_l_list) in
    let s_tcl_size_cent = string_of_int (List.hd !max_tcl_size_cent_list) in
    let s_tcl_size = s_tcl_size_g ^ space ^ s_tcl_size_l ^ space ^ s_tcl_size_cent in 
    write_to_file !file_name (!sample_header ^ space ^ s_nb_component ^ space ^ s_nb_ap ^ space ^ s_modif ^ space ^ s_nb_msg ^ space ^ s_msg_size ^ space ^ s_tcl_size)
  );

  if !prt_full then (
    print_endline ("\n\n##################### ORACLES #######################");
    if traces_equal = 0 then
      print_endline "The centralized and decentralized global traces are equal"
    else
      print_endline "The centralized and decentralized global traces are NOT equal <------------------------------------";

    if local_trace_valid then 
      print_endline "The local trace is valid (i.e. every event has been found as a potential alternative in the global version)"
    else
      print_endline "The local trace could not be verified by the oracles"
  )

let print_stat (s: string) =
  if !file_name <> "" then
    write_to_file !stat_file_name s;
  print_endline s

let formated_string (avg: float) =
  string_of_float (round_generic avg !precision)

let display_full_statistics (nb_test: int) =
  print_endline "\n\n#################################################";
  print_endline "############### FULL STATISTICS  ################";
  print_endline "#################################################";
  
  (* Compute averages and variance *)
  let nb_test_f = float_of_int nb_test in
  let avg_modif_g = (sum_float_list !avg_modif_g_list) /. nb_test_f  in
  let var_modif_g = variance (sum_squared_float_list !avg_modif_g_list) avg_modif_g nb_test in
  let avg_nb_msg_g = (sum_float_list !avg_nb_msg_g_list) /. nb_test_f in
  let var_nb_msg_g = variance (sum_squared_float_list !avg_nb_msg_g_list) avg_nb_msg_g nb_test in
  let avg_msg_size_g = (sum_float_list !avg_msg_size_g_list) /. nb_test_f in
  let var_msg_size_g = variance (sum_squared_float_list !avg_msg_size_g_list) avg_msg_size_g nb_test in
  let avg_tcl_g = compute_avg_tcl_size !max_tcl_size_g_list false in
  let avg_max_tcl_size_g = fst avg_tcl_g in
  let nb_enforcement_g = snd avg_tcl_g in
  let var_max_tcl_size_g = variance (fst (compute_avg_tcl_size !max_tcl_size_g_list true)) avg_max_tcl_size_g (int_of_float nb_enforcement_g) in
  let avg_tr_len_g = float_of_int (sum_int_list !tr_len_g_list) /. nb_test_f in
  let var_tr_len_g = variance (float_of_int (sum_squared_int_list !tr_len_g_list)) avg_tr_len_g nb_test in 

  let avg_modif_l = (sum_float_list !avg_modif_l_list) /. nb_test_f  in
  let var_modif_l = variance (sum_squared_float_list !avg_modif_l_list) avg_modif_l nb_test in
  let avg_nb_msg_l = (sum_float_list !avg_nb_msg_l_list) /. nb_test_f in
  let var_nb_msg_l = variance (sum_squared_float_list !avg_nb_msg_l_list) avg_nb_msg_l nb_test in
  let avg_msg_size_l = (sum_float_list !avg_msg_size_l_list) /. nb_test_f in
  let var_msg_size_l = variance (sum_squared_float_list !avg_msg_size_l_list) avg_msg_size_l nb_test in
  let avg_tcl_l = compute_avg_tcl_size !max_tcl_size_l_list false in
  let avg_max_tcl_size_l = fst avg_tcl_l in
  let nb_enforcement_l = snd avg_tcl_l in
  let var_max_tcl_size_l = variance (fst (compute_avg_tcl_size !max_tcl_size_l_list true)) avg_max_tcl_size_l (int_of_float nb_enforcement_l) in
  let avg_tr_len_l = float_of_int (sum_int_list !tr_len_l_list) /. nb_test_f in
  let var_tr_len_l = variance (float_of_int (sum_squared_int_list !tr_len_l_list)) avg_tr_len_l nb_test in 

  let avg_diff_event = float_of_int (sum_int_list !nb_diff_event_list) /. nb_test_f in
  let avg_diff_nb_modif_d = float_of_int (sum_int_list !nb_diff_nb_modif_list_d) /. nb_test_f in

  let avg_modif_cent = (sum_float_list !avg_modif_cent_list) /. nb_test_f  in
  let var_modif_cent = variance (sum_squared_float_list !avg_modif_cent_list) avg_modif_cent nb_test in
  let avg_nb_msg_cent = (sum_float_list !avg_nb_msg_cent_list) /. nb_test_f in
  let var_nb_msg_cent = variance (sum_squared_float_list !avg_nb_msg_cent_list) avg_nb_msg_cent nb_test in
  let avg_msg_size_cent = (sum_float_list !avg_msg_size_cent_list) /. nb_test_f in
  let var_msg_size_cent = variance (sum_squared_float_list !avg_msg_size_cent_list) avg_msg_size_cent nb_test in
  let avg_tcl_cent = compute_avg_tcl_size !max_tcl_size_cent_list false in
  let avg_max_tcl_size_cent = fst avg_tcl_cent in
  let nb_enforcement_cent = snd avg_tcl_cent in
  let var_max_tcl_size_cent = variance (fst (compute_avg_tcl_size !max_tcl_size_cent_list true)) avg_max_tcl_size_cent (int_of_float nb_enforcement_cent) in
  let avg_tr_len_cent = float_of_int (sum_int_list !tr_len_cent_list) /. nb_test_f in
  let var_tr_len_cent = variance (float_of_int (sum_squared_int_list !tr_len_cent_list)) avg_tr_len_cent nb_test in 

  let avg_diff_event_g = float_of_int (sum_int_list !nb_diff_event_list_cent_g) /. nb_test_f in
  let avg_diff_event_l = float_of_int (sum_int_list !nb_diff_event_list_cent_l) /. nb_test_f in
  let avg_diff_nb_modif_g = float_of_int (sum_int_list !nb_diff_nb_modif_list_cent_g) /. nb_test_f in
  let avg_diff_nb_modif_l = float_of_int (sum_int_list !nb_diff_nb_modif_list_cent_l) /. nb_test_f in

  (* Formated strings *)
  let s_avg_modif_g = formated_string avg_modif_g in
  let s_var_modif_g = formated_string var_modif_g in
  let s_avg_nb_msg_g = formated_string avg_nb_msg_g in
  let s_var_nb_msg_g = formated_string var_nb_msg_g in
  let s_avg_msg_size_g = formated_string avg_msg_size_g in
  let s_var_msg_size_g = formated_string var_msg_size_g in
  let s_avg_max_tcl_size_g = formated_string avg_max_tcl_size_g in
  let s_nb_enforcement_g = string_of_int (int_of_float nb_enforcement_g) in
  let s_var_max_tcl_size_g = formated_string var_max_tcl_size_g in
  let s_avg_tr_len_g = formated_string avg_tr_len_g in
  let s_var_tr_len_g = formated_string var_tr_len_g in

  let s_avg_modif_l = formated_string avg_modif_l in
  let s_var_modif_l = formated_string var_modif_l in
  let s_avg_nb_msg_l = formated_string avg_nb_msg_l in
  let s_var_nb_msg_l = formated_string var_nb_msg_l in
  let s_avg_msg_size_l = formated_string avg_msg_size_l in
  let s_var_msg_size_l = formated_string var_msg_size_l in
  let s_avg_max_tcl_size_l = formated_string avg_max_tcl_size_l in
  let s_nb_enforcement_l = string_of_int (int_of_float nb_enforcement_l) in
  let s_var_max_tcl_size_l = formated_string var_max_tcl_size_l in
  let s_avg_tr_len_l = formated_string avg_tr_len_l in
  let s_var_tr_len_l = formated_string var_tr_len_l in

  let s_avg_diff_event = formated_string avg_diff_event in
  let s_avg_diff_nb_modif_d = formated_string avg_diff_nb_modif_d in

  let s_avg_modif_cent = formated_string avg_modif_cent in
  let s_var_modif_cent = formated_string var_modif_cent in
  let s_avg_nb_msg_cent = formated_string avg_nb_msg_cent in
  let s_var_nb_msg_cent = formated_string var_nb_msg_cent in
  let s_avg_msg_size_cent = formated_string avg_msg_size_cent in
  let s_var_msg_size_cent = formated_string var_msg_size_cent in
  let s_avg_max_tcl_size_cent = formated_string avg_max_tcl_size_cent in
  let s_nb_enforcement_cent = string_of_int (int_of_float nb_enforcement_cent) in
  let s_var_max_tcl_size_cent = formated_string var_max_tcl_size_cent in
  let s_avg_tr_len_cent = formated_string avg_tr_len_cent in
  let s_var_tr_len_cent = formated_string var_tr_len_cent in

  let s_avg_diff_event_g = formated_string avg_diff_event_g in
  let s_avg_diff_event_l = formated_string avg_diff_event_l in
  let s_avg_diff_nb_modif_g = formated_string avg_diff_nb_modif_g in
  let s_avg_diff_nb_modif_l = formated_string avg_diff_nb_modif_l in
  
  print_stat ("\n############## DECENTRALIZED VERSION ################");
  print_stat "               ------- GLOBAL ------";
  print_stat ("Average number of modifications: " ^ s_avg_modif_g ^ "          Variance: " ^ s_var_modif_g);
  print_stat ("Average length of the trace: " ^ s_avg_tr_len_g ^ "          Variance: " ^ s_var_tr_len_g);
  print_stat ("Average number of messages: " ^ s_avg_nb_msg_g ^ "          Variance: " ^ s_var_nb_msg_g);
  print_stat ("Average size of the messages: " ^ s_avg_msg_size_g ^ "          Variance: " ^ s_var_msg_size_g);
  print_stat ("Average maximal size of the tcl: " ^ s_avg_max_tcl_size_g ^ "          Variance: " ^ s_var_max_tcl_size_g);
  if !prt_full then print_endline ("Number of runs in which the alternatives were computed at least once: " ^ s_nb_enforcement_g);
  print_stat "\n               ------- LOCAL -------";
  print_stat ("Average number of modifications: " ^ s_avg_modif_l ^ "          Variance: " ^ s_var_modif_l);
  print_stat ("Average length of the trace: " ^ s_avg_tr_len_l ^ "          Variance: " ^ s_var_tr_len_l);
  print_stat ("Average number of messages: " ^ s_avg_nb_msg_l ^ "          Variance: " ^ s_var_nb_msg_l);
  print_stat ("Average size of the messages: " ^ s_avg_msg_size_l ^ "          Variance: " ^ s_var_msg_size_l);
  print_stat ("Average maximal size of the tcl: " ^ s_avg_max_tcl_size_l ^ "          Variance: " ^ s_var_max_tcl_size_l);
  if !prt_full then print_endline ("Number of runs in which the alternatives were computed at least once: " ^ s_nb_enforcement_l);
  print_stat ("\nAverage number of different events between the enforced traces (comparing global and local version): " ^ s_avg_diff_event);
  print_stat ("Average number of event with a different number of modifications between the enforced traces (comparing global and local version): " ^ s_avg_diff_nb_modif_d);
  print_stat ("\n############### CENTRALIZED VERSION #################");
  print_stat ("Average number of modifications: " ^ s_avg_modif_cent ^ "          Variance: " ^ s_var_modif_cent);
  print_stat ("Average length of the trace: " ^ s_avg_tr_len_cent ^ "          Variance: " ^ s_var_tr_len_cent);
  print_stat ("Average number of messages: " ^ s_avg_nb_msg_cent ^ "          Variance: " ^ s_var_nb_msg_cent);
  print_stat ("Average size of the messages: " ^ s_avg_msg_size_cent ^ "          Variance: " ^ s_var_msg_size_cent);
  print_stat ("Average maximal size of the tcl: " ^ s_avg_max_tcl_size_cent ^ "          Variance: " ^ s_var_max_tcl_size_cent);
  if !prt_full then print_endline ("Number of runs in which the alternatives were computed at least once: " ^ s_nb_enforcement_cent);
  print_stat ("\nAverage number of different events between the enforced traces");
  print_stat ("                                   - Comparing the centralized and decentralized global version: " ^ s_avg_diff_event_g);
  print_stat ("                                   - Comparing the centralized and decentralized local version: " ^ s_avg_diff_event_l);
  print_stat "\nAverage number of event with a different number of modifications between the enforced traces";
  print_stat ("                                   - Comparing the centralized and decentralized global version: " ^ s_avg_diff_nb_modif_g);
  print_stat ("                                   - Comparing the centralized and decentralized local version: " ^ s_avg_diff_nb_modif_l);
  if !prt_full then (
    if !nb_diff_traces = 1 then
      print_endline ("\nThere is 1 run in which the centralized and decentralized global trace are not equal")
    else if !nb_diff_traces > 1 then
      print_endline ("\nThere are " ^ string_of_int !nb_diff_traces ^ " runs in which the centralized and decentralized global trace are not equal");
    if !nb_invalid_local_tr = 1 then
      print_endline ("\nThere is 1 run in which the decentralized local trace could not be verified by the oracle")
    else if !nb_invalid_local_tr > 1 then
      print_endline ("\nThere are " ^ string_of_int !nb_invalid_local_tr ^ " runs in which the decentralized local trace could not be verified by the oracle")
  );

  if !produce_tex then (
    write_to_file "STATS_TEX_1.txt" 
      ("Cent. & & " ^ s_avg_modif_cent ^ " & " ^ s_avg_nb_msg_cent ^ " & " ^ s_avg_msg_size_cent ^ " & " ^ s_avg_max_tcl_size_cent ^ " \\\\
      \\cmidrule{1-1}\\cmidrule{3-6}\\cmidrule{9-12}
      Global & " ^ !sample_header ^ " & " ^ s_avg_modif_g ^ " & " ^ s_avg_nb_msg_g ^ " & " ^ s_avg_msg_size_g ^ " & " ^ s_avg_max_tcl_size_g ^ " \\\\
      \\cmidrule{1-1}\\cmidrule{3-6}\\cmidrule{9-12}
      Local & & " ^ s_avg_modif_l ^ " & " ^ s_avg_nb_msg_l ^ " & " ^ s_avg_msg_size_l ^ " & " ^ s_avg_max_tcl_size_l ^ " \\\\");
    write_to_file "STATS_TEX_2.txt"
      (!sample_header ^ " & " ^ s_avg_diff_event_g ^ " & " ^ s_avg_diff_event_l ^ " & " ^ s_avg_diff_event ^ " & " ^ s_avg_diff_nb_modif_g ^ " & " ^ s_avg_diff_nb_modif_l ^ " & " ^ s_avg_diff_nb_modif_d ^ " \\\\")
  )
    
let test_irrelevant_ap () =
  let phi : ltl = Glob (And (Var "a", Var "c")) in
  let alpha : d_alphabet = [["a"]; ["b"]; ["c"]] in
  let event = [["a"]; ["b"]; []] in
  complete_test phi alpha [event]

let init_global_var (prt_full_stats: bool) (preci: int) (file: string) (header: string) (tex: bool)=
  prt_full := prt_full_stats;
  precision := preci;
  file_name := file;
  stat_file_name := "STATS_" ^ !file_name;
  sample_header := header;
  produce_tex := tex; 
  if !produce_tex then (
    if !file_name <> "" then write_to_file "STATS_TEX_1.txt" "\\midrule";
    if !file_name <> "" then write_to_file "STATS_TEX_2.txt" "\\midrule";
  );
  avg_modif_g_list := [];
  avg_nb_msg_g_list := [];
  avg_msg_size_g_list := [];
  max_tcl_size_g_list := [];
  tr_len_g_list := [];
  avg_modif_l_list := [];
  avg_nb_msg_l_list := [];
  avg_msg_size_l_list := [];
  max_tcl_size_l_list := [];
  tr_len_l_list := [];
  nb_diff_event_list := [];
  nb_diff_nb_modif_list_d := [];
  avg_modif_cent_list := [];
  avg_nb_msg_cent_list := [];
  avg_msg_size_cent_list := [];
  max_tcl_size_cent_list := [];
  tr_len_cent_list := [];
  nb_diff_event_list_cent_g := [];
  nb_diff_event_list_cent_l := [];
  nb_diff_nb_modif_list_cent_g := [];
  nb_diff_nb_modif_list_cent_l := [];
  nb_diff_traces := 0;
  nb_invalid_local_tr := 0

(* Test using a randomly generated formula and trace. *)
let rec test_random (nb_test: int) (alpha: d_alphabet) (size_f: int) (size_tr: int) (bias: bool) =
  if !prt_full then
    print_endline "\n#################################################\n#################################################";

  match nb_test with
      0 -> ()
    | _ -> (
      let f = (
        if bias then
          gen_1_form_nontrivial_biased size_f alpha (Random.int (length alpha))
        else
          gen_1_form_nontrivial size_f (List.concat alpha)
      ) in
      let trace = gen_1_dtrace size_tr alpha in
      complete_test f alpha trace;
      test_random (nb_test - 1) alpha size_f size_tr bias
    )

let test_random_formula (nb_test: int) (alpha: d_alphabet) (size_f: int) (size_tr: int) (bias: bool) (preci: int) (prt_full_stats: bool) (file: string) (produce_tex: bool) =
  let seed = Random.int 10000 in
  Random.init seed;
  init_global_var prt_full_stats preci file (string_of_int size_f) produce_tex;
  test_random nb_test alpha size_f size_tr bias;
  display_full_statistics nb_test;
  print_endline ("\nSeed: " ^ string_of_int seed)

let test_random_formula_seeded (seed: int) (nb_test: int) (alpha: d_alphabet) (size_f: int) (size_tr: int) (bias: bool) (preci: int) (prt_full_stats: bool) (file: string) (produce_tex: bool) =
  Random.init seed;
  init_global_var prt_full_stats preci file (string_of_int size_f) produce_tex;
  test_random nb_test alpha size_f size_tr bias;
  display_full_statistics nb_test

let rec test_pattern_rec (kind: string) (nb_test: int) (alpha: d_alphabet) (size_tr: int) =
  if !prt_full then
    print_endline "\n#################################################\n#################################################";

  match nb_test with
      0 -> ()
    | _ -> (
      let f: ltl = (
        if kind="abs" then (
          gen_1_form_abscence (globalAlphabet alpha)
        ) else (
        if kind="exist" then (
          gen_1_form_existence (globalAlphabet alpha)
        ) else (
        if kind="bexist" then (
          gen_1_form_boundedexistence (globalAlphabet alpha)
        ) else (
        if kind="unive" then (
          gen_1_form_universality (globalAlphabet alpha)
        ) else (
        if kind="prec" then (
          gen_1_form_precedence (globalAlphabet alpha)
        ) else (
        if kind="resp" then (
          gen_1_form_response (globalAlphabet alpha)
        ) else (
        if kind="pchain" then (
          gen_1_form_precedence_chain (globalAlphabet alpha)
        ) else (
        if kind="rchain" then (
          gen_1_form_response_chain (globalAlphabet alpha)
        ) else (
          gen_1_form_constrained_chain (globalAlphabet alpha)
        ))))))))
      ) in
      let trace = gen_1_dtrace size_tr alpha in
      complete_test f alpha trace;
      test_pattern_rec kind (nb_test - 1) alpha size_tr
    )

let test_pattern (kind: string) (nb_test: int) (alpha: d_alphabet) (size_tr: int) (preci: int) (prt_full_stats: bool) (file: string) (produce_tex: bool) =
  let seed = Random.int 10000 in
  Random.init seed;
  init_global_var prt_full_stats preci file kind produce_tex;
  test_pattern_rec kind nb_test alpha size_tr;
  display_full_statistics nb_test;
  print_endline ("\nSeed: " ^ string_of_int seed)


let test_pattern_seeded (seed: int) (kind: string) (nb_test: int) (alpha: d_alphabet) (size_tr: int) (preci: int) (prt_full_stats: bool) (file: string) (produce_tex: bool) =
  Random.init seed;
  init_global_var prt_full_stats preci file kind produce_tex;
  test_pattern_rec kind nb_test alpha size_tr;
  display_full_statistics nb_test

let rec test_specific_rec (phi_l: ltl list) (alpha: d_alphabet) (size_tr: int) =
  if !prt_full then
    print_endline "\n#################################################\n#################################################";

  match phi_l with
      hd::tl -> (
        let trace = gen_1_dtrace size_tr alpha in
        complete_test hd alpha trace;
        test_specific_rec tl alpha size_tr
      )
    | _ -> ()

let test_specific (phi_l: ltl list) (alpha: d_alphabet) (size_tr: int) (preci: int) (prt_full_stats: bool) (file: string) (produce_tex: bool) =
  let seed = Random.int 10000 in
  Random.init seed;
  init_global_var prt_full_stats preci file "" produce_tex;
  test_specific_rec phi_l alpha size_tr;
  display_full_statistics (List.length phi_l)

let test_specific_seeded (seed: int) (phi_l: ltl list) (alpha: d_alphabet) (size_tr: int) (preci: int) (prt_full_stats: bool) (file: string) (produce_tex: bool) =
  Random.init seed;
  init_global_var prt_full_stats preci file "" produce_tex;
  test_specific_rec phi_l alpha size_tr;
  display_full_statistics (List.length phi_l)