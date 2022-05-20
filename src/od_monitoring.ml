open Batteries
open Utils
open DynArray
open Alphabetevent
open Trace
open Dltl
open Architecture
open Common_network
open Network
open Dyn_network
open Common_monitoring
open Od_progression

(** stat variables **)
let nb_messages = ref 0
let size_messages = ref 0.
let nb_progressions = ref 0
(** end stat variables **)
    
type message_kind = Value of coordinate * coordinate * truth_value * time_instant | Kill of coordinate * time_instant
type message = coordinate *  message_kind
    
(** Conversion of truth-value to Dltl **)
let to_dltl (v:truth_value) : dltl =
  match v with
    | True -> True
    | False -> False
    | _ -> failwith "This should not happen: truth-value Undetermined cannot be converted to a DLtl formula"

(** Conversion of Dltl to truth-value **)
let to_truth_value (f:dltl) : truth_value =
  match f with
    | True -> True
    | False -> False  
    | _ -> Undeter

      
let stringrep_messagekind (k:message_kind) : string =
  match k with
    | Value (x,y,v,t) -> "Value("^(stringrep_coordinates (x,y))^", "^(stringrep_truthvalue v)^", "^(stringrep_timeinstant t)
    | Kill (c,t) -> "kill("^(stringrep_coordinate c)^", "^(stringrep_timeinstant t)^")"
      
let stringrep_message (m:message) : string =
  let (c, k) = m in
  let body = (stringrep_coordinate c)^", "^(stringrep_messagekind k) in
  "("^body^")"
      
(**////////////////////////////////////////////////////////////////
   Accessor of messages
   ////////////////////////////////////////////////////////// **)

(** Coordinates of a message **)
let destination_of_message ((c,_): message) : coordinate =
  c

(** Time instant of a message **)
let time_instant_of_message ((_,k):message) : time_instant =
  match k with
    | Value (_,_,_,t) -> t
    | Kill (_,t) -> t

(** Value of message **)
let value_of_message ((_,k):message) : truth_value =
  match k with
    | Value (_,_,v,_) -> v
    | _ -> failwith "This should not happen, messages does not contain a value, it is a kill message"



(**////////////////////////////////////////////////////////////////
   Mail boxes
   ////////////////////////////////////////////////////////// **)

type inbox = message list
type outbox = message list

type mailbox = inbox * outbox
type network_mail = mailbox array

let stringrep_inbox (ib:inbox) : string =
  "in:"^(stringrep_list stringrep_message ib)

let stringrep_outbox (ob:outbox) : string =
  "out:"^stringrep_list stringrep_message ob

let stringrep_mailbox (mb:mailbox) : string =
  let (ib,ob) = mb in
  let body = (stringrep_inbox ib)^", "^(stringrep_outbox ob)
  in "(" ^ body ^")"

let stringrep_networkmail (m:network_mail) : string =
  stringrep_array m stringrep_mailbox true
    
(**////////////////////////////////////////////////////////////////
   Monitoring initialization and pointers
   ////////////////////////////////////////////////////////// **)
    
let current_time : int ref = ref 0
    
let is_init_verbose : bool ref = ref false

let init_message (msg:string) : unit =
  if (!is_init_verbose)
  then
    print_endline (msg)
      
let empty_cell = Cell.empty
let empty_dynnet_loc = Array.make 0 empty_cell
let empty_dynnet = Array.make 0 empty_dynnet_loc
    
let the_network : dyn_net ref = ref empty_dynnet
(** The initial version of the network, kept as a copy for automatically respawning formulae **)
let init_network : dyn_net ref = ref empty_dynnet
let the_network_size : int ref = ref 0
let the_main_cell : (int * int) ref = ref (0,0)
let the_mail : network_mail ref = ref (Array.make 0 ([],[]))

let initialize_mailboxes (net:network) : unit =
  let size = Network.cardinal net in
  the_mail := Array.make size (([],[]))

let the_referents : referents_network ref = ref (Array.make 0 [])
let the_referrers : referrers_network ref =  ref (Array.make 0 (Array.make 0 []))  

let the_respawns : bool array array ref = ref (Array.make 0 (Array.make 0 false))

let add_to_outmail (m:message list) (index:coordinate) : unit =
  let (existing_in,existing_out) : mailbox = (!the_mail).(index) in
  (!the_mail).(index) <- (existing_in, existing_out@m)

let add_to_inmail (m:message list) (index:coordinate) : unit =
  let (existing_in,existing_out) : mailbox = (!the_mail).(index) in
  (!the_mail).(index) <- (existing_in@m, existing_out)

(** Size of a message **)
let size_of_message ((_,k): message) : float =
  let log2 (f:float) : float =
    log f /. log 2. in
  let bits_for_coord = ceil (log2 (float_of_int (!the_network_size))) in
  match k with
    | Value (x,y,v,t) -> 2. *. bits_for_coord +. 1. +. ceil (log2 (float_of_int (if t> 1 then t else 1)))
    | Kill (_, t) -> bits_for_coord +. ceil (log2 (float_of_int t))
    
let send_mail () : unit =
  let size_messages_tmp = ref 0. in
  let send_mail_component (index:coordinate) : unit =
    let (in_messages, out_messages) = !the_mail.(index) in
    let out_messages = suppress_duplicates out_messages in
    nb_messages := !nb_messages + (List.length out_messages);
    List.iter (fun (d, k) -> add_to_inmail [(d,k)] d) out_messages;
    List.iter (fun m -> size_messages_tmp := !size_messages_tmp +. size_of_message m) out_messages;
    !the_mail.(index) <- (in_messages, []) in
  size_messages_tmp := 0.;
  for i = 0 to !the_network_size - 1 do
    send_mail_component i
  done;
  size_messages := !size_messages +. !size_messages_tmp /. float_of_int (!the_network_size)
    
  
let compute_the_refererents_and_referrers (net:network) : unit =
  the_referents := referents_of_network net;
  the_referrers := referrers_of_network net
  
let compute_respawning_cells (archi:architecture) (net:network) (phi:dltl) : unit =
  let respawn_list = compute_respawn archi net phi in
  let size = Network.cardinal net in
  the_respawns := Array.make size (Array.make 0 false);
  for cpt = 0 to size - 1 do
    let the_length = length (Network.find cpt net) in
    !the_respawns.(cpt) <- Array.make the_length false;
  done;
  List.iter (fun (i,j) -> !the_respawns.(i).(j) <- true) respawn_list
  
let init_monitoring (phi:dltl) (archi:architecture) : unit =
  init_message ("START initialization of monitoring of\n"^(string_rep phi)^"\n on \n"^(stringrep_architecture archi));
  nb_messages := 0;
  size_messages := 0.;
  nb_progressions := 0;
  let chc_phi = chc phi archi 0 in
  init_message ("Chosen component for the main formula: "^(string_of_int chc_phi));
  let create_static_network : network =
    let empty_network = make_empty_network (Architecture.cardinal archi) in
    let net = network archi chc_phi empty_network phi in
    init_message ("Initial static network:\n"^(stringrep_network net));
    let compactnet = compact net in
    init_message ("Compacted static network:\n"^(stringrep_network compactnet));
    reindex compactnet in
  let net = create_static_network in
  the_main_cell := main_coord archi net phi;
  init_message ("The main cell is at coordinates: "^(stringrep_coordinates !the_main_cell));
  init_message ("Final static network:\n"^(stringrep_network net));
  compute_respawning_cells archi net phi;
  init_message ("Respawning cells:\n"^(stringrep_arrayarray !the_respawns string_of_bool));
  compute_the_refererents_and_referrers net;
  init_message ("Referents:\n"^(stringrep_referents_network !the_referents));
  init_message ("Referrers:\n"^(stringrep_arrayarray !the_referrers stringrep_int_list));
  let dynnet = make_dyn_net net and dynnet_init = make_dyn_net net in
  the_network := dynnet;
  init_network := dynnet_init;
  the_network_size := Array.length !the_network;

  init_message ("Created dynamic network:\n"^(stringrep_dyn_net !the_network));
  initialize_mailboxes net;
  init_message ("Initialized mailboxes");
  current_time := 0;
  init_message ("END initialization of monitoring.")

    
(**////////////////////////////////////////////////////////////////
   Auxiliary functions for monitoring
   ////////////////////////////////////////////////////////// **)

let display_network () =
  monitoring_message ("NETWORK:");
  monitoring_message (stringrep_dyn_net !the_network)
  
let display_mailboxes () =
  monitoring_message ("MAILBOXES:");
  monitoring_message (stringrep_networkmail !the_mail)

let display_network_full () =
  monitoring_message ("NETWORK:");
  monitoring_message (stringrep_dyn_net !the_network);
  monitoring_message ("MAILBOXES:");
  monitoring_message (stringrep_networkmail !the_mail)

(** Apply substitution of pointer by a truth-value in a dyn_net_loc **)
let substitute_pointer_by_truth_value_loc (l:dyn_net_loc) (i:int) (j:int) (v:truth_value) (t:time_instant) : dyn_net_loc =
  (** Apply substitution of pointer by a truth-value in a cell **)
  let substitute_pointer_by_truth_value_cell (c:cell) : cell =
    (** Apply substitution of pointer by a truth-value in a formula **)
    let substitute_pointer_by_truth_value_formula (f:dltl) : dltl =
      let rec sub (f:dltl):dltl =
	match f with
	  | Or (fp, fpp) -> Or (sub fp, sub fpp)
	  | And (fp, fpp) -> And (sub fp, sub fpp)
	  | Glob fp -> Glob (sub fp)
	  | Neg fp -> Neg (sub fp)
	  | Ev fp -> Ev (sub fp)
	  | Next fp -> Next (sub fp)
	  | Distd (x, y, tt) when (x = i && y = j && t = tt) -> if v = True then True else False 
	  | Until (fp, fpp) -> Until (sub fp, sub fpp)
	  | f -> f
      in sub f in
    Cell.map (fun elt -> substitute_pointer_by_truth_value_formula elt) c in
  Array.map (fun elt -> substitute_pointer_by_truth_value_cell elt) l

let get_main_cell () =
  let x,y = !the_main_cell in
  !the_network.(x).(y)

(**////////////////////////////////////////////////////////////////
   Monitoring functions
   ////////////////////////////////////////////////////////// **)
  
let treat_value_messages (index:coordinate) (inbox: inbox) : unit =
  let updated_loc : dyn_net_loc = List.fold_left
    (fun acc elt -> match elt with | (_, Value(x,y,v,t)) -> substitute_pointer_by_truth_value_loc acc x y v t | _ -> failwith "this should not happen")
    !the_network.(index) inbox in
  !the_network.(index) <- updated_loc

let treat_kill_messages (index:coordinate) (inbox: inbox) : unit =
  let indexes : (int*int) list = List.map (fun elt -> match elt with (_,Kill(x,y)) -> (x,y) | _ -> failwith "") inbox in
  let treat_one_kill_message (sender:coordinate) (cindex:coordinate) : unit =
    let cell_referrers = !the_referrers.(index).(cindex) in
    if (not (List.is_empty cell_referrers)) then
      (
	let new_cell_referrers = List.filter (fun r -> r <> sender) cell_referrers in
	!the_referrers.(index).(cindex) <- new_cell_referrers;
	if List.is_empty new_cell_referrers then
	  !the_network.(index).(cindex) <- Cell.empty
      )
  in
  List.iter (fun (x,y) -> treat_one_kill_message x y) indexes

let check_inmail (index:coordinate) : unit =
  let (inmail,outmail) = !the_mail.(index) in
  let (vmsgs,kmsgs) = List.partition (fun (_,k) -> match k with Value _ -> true | _ -> false) inmail in
  !the_mail.(index) <- ([], outmail);
  if vmsgs <> [] then
    (
      treat_value_messages index vmsgs
    );
  if kmsgs <> [] then
    (
      treat_kill_messages index kmsgs
    )
      
let progress_every_formula (index:coordinate) (sigma:event) : unit =
  let location = !the_network.(index) in
  for cpt = 0 to (Array.length location) - 1 do
    location.(cpt) <- Cell.map (fun elt -> nb_progressions := !nb_progressions + count_needed_progressions_dltl elt;(progress elt sigma !current_time)) location.(cpt) 
  done
    
let simplify_formulae (index:coordinate) : unit =
   let location = !the_network.(index) in
  for cpt = 0 to (Array.length location) - 1 do
    location.(cpt) <- Cell.map (fun elt -> simp elt) location.(cpt) 
  done

let simplify_formulae_with_substitution (index:coordinate) : unit =
   let location = !the_network.(index) in
  for cpt = 0 to (Array.length location) - 1 do
    location.(cpt) <- Cell.map (fun elt -> simplify_by_dereferring elt) location.(cpt) 
  done
    
let checking_for_referrers (index:coordinate) (cellindex:coordinate) : unit =
  let cell_referrers = !the_referrers.(index).(cellindex) in
  let the_cell = !the_network.(index).(cellindex) in
  let checking_for_formula (t:time_instant) (formula:dltl) : unit =
    if (formula = True || formula = False) then
      (
	let tv = to_truth_value formula in
	let out_messages : message list = List.map (fun r -> (r , Value (index, cellindex, tv, t))) cell_referrers in
	add_to_outmail (suppress_duplicates out_messages) index;
      )
  in
  Cell.iter (fun t f -> checking_for_formula t f) the_cell
  (**;
  let new_cell = Cell.fold
    (fun (k:time_instant) (form:dltl) acc ->
      if (((index,cellindex) <> !the_main_cell) && not (!the_respawns.(index).(cellindex)) && (form = True || form = False)) then
	Cell.remove k acc else acc
    )
    the_cell the_cell in
  !the_network.(index).(cellindex) <- new_cell
  **)
let checking_for_referents (index:coordinate) : unit =
  let last_referents : referents = !the_referents.(index) in
  let referents_cell : referents = Array.fold_left (fun acc elt -> acc@(Cell.fold (fun _ f l -> l@(referents_of_formula f)) elt []))
    [] !the_network.(index) in
  let the_static_referents = Array.fold_left (fun acc elt -> acc@(Cell.fold (fun _ f l -> l@(referents_of_formula f)) elt []))
    [] !init_network.(index) in
  let referents_cell = suppress_duplicates (referents_cell@the_static_referents) in
  let (new_referents, old_referents) = List.partition (fun elt -> List.mem elt referents_cell) last_referents in
  !the_referents.(index) <- new_referents;
  (* notify the other components with kill messages that the cell is not needed anymore *)
  (* I do not handle time instants for the moment, so the value is 0 *)
  let out_messages : message list= List.map (fun (x,y) -> (x, Kill (index,y))) old_referents in
  add_to_outmail out_messages index
 
let respawn_cells (index:coordinate) : unit =
  let respawn_cell (i:coordinate) (cell:cell) : unit =
    if (!the_respawns.(index).(i)) then
      let new_cell = Cell.add (!current_time+1) (snd (Cell.choose !init_network.(index).(i))) cell in
      !the_network.(index).(i) <- new_cell
(**	else
	(
	let new_cell = Cell.add !current_time phi cell in
	let new_cell = Cell.remove t new_cell in
	!the_network.(index).(i) <- new_cell
	)
**)     in
  Array.iteri (fun i cell -> respawn_cell i cell) !the_network.(index)

(**
Progresses the indexes of a cell
NOT USED as it is not actually needed, I thought it was needed, but should not be done
**) 
let progress_indexes2 (index:coordinate) : unit =
  let prog_indexes (i:coordinate) (cell:cell) : unit =
    if (not (Cell.is_empty cell)) then
      (
	let (t,phi) = Cell.max_binding cell in
	if (not !the_respawns.(index).(i)) then
	  (
	    let new_cell = Cell.add !current_time phi cell in
	    let new_cell = Cell.remove t new_cell in
	    !the_network.(index).(i) <- new_cell
	  )
      )
  in
  Array.iteri (fun i cell -> prog_indexes i cell) !the_network.(index)

let check_for_referrers (index:coordinate) =
  for cpt = 0 to Array.length (!the_network.(index)) - 1 do
    checking_for_referrers index cpt
  done

let check_for_referrents_and_referrers (index:coordinate) : unit =
  check_for_referrers index;
  checking_for_referents index


      
let garbage_collect_component (index:coordinate) : unit =
  let garbage_collect_cell (cell_index:coordinate) (the_cell:cell) : unit =
    if ((index,cell_index) <> !the_main_cell) then
      (
	!the_network.(index).(cell_index) <- Cell.fold (fun k e acc -> if (e = Dltl.True || e = Dltl.False) then acc else Cell.add k e acc) the_cell Cell.empty  
      )
  in
  Array.iteri (fun i c -> garbage_collect_cell i c) !the_network.(index)

  (**
    let new_cell = Cell.fold
    (fun (k:time_instant) (form:dltl) acc ->
      if (((index,cellindex) <> !the_main_cell) && not (!the_respawns.(index).(cellindex)) && (form = True || form = False)) then
	Cell.remove k acc else acc
    )
    the_cell the_cell in
    !the_network.(index).(cellindex) <- new_cell
    **)
    
let make_one_monitoring_step_component (index:coordinate) (sigma:event) : unit =
  check_inmail index;
  simplify_formulae index;
  simplify_formulae_with_substitution index;
  check_for_referrents_and_referrers index;
  progress_every_formula index sigma;
  simplify_formulae index;
  simplify_formulae_with_substitution index;
  check_for_referrents_and_referrers index;
  garbage_collect_component (index)


let check_for_final_verdict () : unit =
  let main_cell = get_main_cell () in
  let formula_verdict = ref SHARP in
  let size_cell = Cell.cardinal main_cell in
  for cpt = 0 to size_cell - 1 do
    if (!formula_verdict = SHARP) then
      (
	let form = Cell.find cpt main_cell in
	if (form = True || form = False) then
	  formula_verdict := form
      )
  done;
  if (!formula_verdict = True || !formula_verdict = False) then
    (
      (** TODO: add verdict message to the outbox **)
      let tv = to_truth_value !formula_verdict in
      raise (End_of_Monitoring(!current_time, tv))
    )

let increment_time_stamp_of_main_formula () : unit =
  let main_cell : cell = get_main_cell () in
  let (t,f) : time_instant * dltl = Cell.choose main_cell in
  let (mainx,mainy) : coordinate * coordinate = !the_main_cell in
  let new_main_cell = (Cell.add (t+1) f main_cell) in
  !the_network.(mainx).(mainy) <- new_main_cell
      
let make_one_monitoring_step (sigma:event) : unit =
  display_mailboxes ();
  monitoring_message ("*** MAIL exchange");
  send_mail ();
  display_mailboxes ();
  monitoring_message ("*** Begin Processing event "^stringrep_event sigma^" at time: "^(string_of_int !current_time));
  display_network ();
  for cpt = 0 to !the_network_size - 1 do
    make_one_monitoring_step_component cpt sigma
  done;
  monitoring_message ("Each component has done its step function.");
  display_network ();  
  check_for_final_verdict ();
  monitoring_message ("Referents:\n"^(stringrep_referents_network !the_referents));
  monitoring_message ("Refererrs:\n"^(stringrep_arrayarray !the_referrers stringrep_int_list));
  monitoring_message ("Respawn");
  for cpt = 0 to !the_network_size - 1 do
    respawn_cells cpt;
  done;
(**  increment_time_stamp_of_main_formula (); **)
  display_network ();
  monitoring_message ("*** End Processing event "^stringrep_event sigma^" at time: "^(string_of_int !current_time))

let end_of_monitoring (tv: truth_value) : truth_value =
  monitoring_message ("The final verdict obtained at time "^(string_of_int !current_time)^" is: "^(stringrep_truthvalue tv));
  monitoring_message ("END READING EVENTS");
  monitoring_message ("The final network is:");
  display_network ();
  monitoring_message ("END D_MONITORING");
  tv
    
let od_monitor (archi:architecture) (phi:dltl) (t:trace) : truth_value =
  monitoring_message ("BEGIN MONITORING");
  init_monitoring phi archi;
  monitoring_message (stringrep_dyn_net !the_network);
  monitoring_message ("BEGIN READING EVENTS");
  try
    List.iter (fun elt -> make_one_monitoring_step elt; current_time := !current_time + 1;) t;
      end_of_monitoring (Undeter)
  with
    | End_of_Monitoring (t, v) -> end_of_monitoring (v)

     
      
let od_monitor_stats (archi:architecture) (phi:dltl) (t:trace) : truth_value * trace * int * float * int =
  let end_of_monitoring_stats (tv: truth_value) : truth_value * trace * int * float * int =
    monitoring_message ("The final verdict obtained at time "^(string_of_int !current_time)^" is: "^(stringrep_truthvalue tv));
    monitoring_message ("END READING EVENTS");
    monitoring_message ("The final network is:");
    display_network ();
    monitoring_message ("END D_MONITORING");
    (tv, prefix !current_time t, !nb_messages, !size_messages /. (float_of_int (Utils.max 1 !current_time)), !nb_progressions) in
  monitoring_message ("BEGIN MONITORING");
  init_monitoring phi archi;
  monitoring_message (stringrep_dyn_net !the_network);
  monitoring_message ("BEGIN READING EVENTS");
  try
    List.iter (fun elt -> make_one_monitoring_step elt; current_time := !current_time + 1;) t;
    end_of_monitoring_stats (Undeter)
  with
    | End_of_Monitoring (t, v) -> end_of_monitoring_stats (v)


