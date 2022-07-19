open Alphabetevent
open Trace
open Md_progression
open Common_monitoring
open Utils
open Ltl
open List

let nb_progressions = ref 0

(* Computes the urgency of a formula.
 Note that this function supposes that in a subformula there is no future operator after a previous operator as it is done in the paper.
 *)
let rec urgency (formula:ltl) =
  match formula with
      Previous f -> 1 + urgency f
    | And (a,b) | Or (a,b) | Imp (a,b) | Iff (a,b) -> max (urgency a) (urgency b)
    | Neg f -> urgency f
    | _ -> 0

(* Returns the index of an element in a list *)
let rec index elt the_list =
  match the_list with
      [] -> failwith "index"
    | e::rem -> if e=elt then 0 else 1+index elt rem
		
exception Not_Urgent_Formula of ltl

(* Returns the most urgent obligation in a formula, i.e., the subformula that is the most urgent one according to the definition of urgency.
Note that a trick is used here for efficiency.
Instead of computing the closure of the formula and computing the urgency of each formula in the closure, we rather compute a smaller set.
 This set is the urgency closure where non-urgent formulae are syntactically filtered.   *)
let most_urgent_obligation (formula:ltl) = 
  let uf = urgency formula in
  if uf = 0 then SHARP
  else
    let clos = urgent_closure formula in
    let clos_mou = List.filter (fun x -> urgency x = uf) clos in
    let clos_sort = List.fast_sort (fun x y -> (size y) - (size x)) clos_mou in
    List.hd clos_sort
		

exception Component_Of_Formula_Error

let component_of_var (var:ltl) (alpha:d_alphabet) =
  match var with
      Var var_name -> 
	(let alphai = List.find (fun x -> List.mem var_name x) alpha in
	 index alphai alpha )
    | _ -> raise Component_Of_Formula_Error


let component_of_formula (form:ltl) (alpha:d_alphabet) (avoid:int)=
  let vars = variables form in
  let indexes = List.map (fun x -> component_of_var x alpha) vars in
  let avoided = List.filter (fun x -> x <> avoid) indexes in
  if (List.length avoided <> 0) then
    List.nth avoided (Random.int (List.length avoided))
  else avoid

let component_of_most_urgent_obligation (formula:ltl) (alpha:d_alphabet) = 
  let uf = urgency formula in
  let clos = urgent_closure formula in
  let clos_mou = List.filter (fun x -> urgency x = uf) clos in
  let vars = List.flatten(List.map variables clos_mou) in
  let compos = List.map (fun x -> component_of_var x alpha) vars in
  let compos_sort = List.fast_sort (fun x y -> x - y) compos in
  List.hd compos_sort

(** Computes the size of a message. Here a message is an LTL
    formula. To do so, we compute the number of characters necessary
    to encode the formula and multiply by log_2 16 where 16 is the
    number of LTL symbols **)
let size_of_message (f:ltl) : int =
  4 * nb_characters_in_encoding f

(* Computes the obligations of the formulae in an array
*)
let compute_obligations (formulae: ltl array) (alpha:d_alphabet)= 
  let nmessages = ref 0 and smessages = ref 0 in
  let formulaeArray = Array.map simp formulae in 
  let result = Array.make (Array.length formulae) SHARP in 
  let mou = Array.init (Array.length formulae) (fun i -> most_urgent_obligation formulaeArray.(i)) in
  for i=0 to (Array.length result) -1 do
    if (mou.(i) <> SHARP) then
      try
	let index = component_of_most_urgent_obligation formulaeArray.(i) alpha in
	smessages := !smessages + (size (formulae.(i)));
	result.(index) <- simp (And (formulae.(i), result.(index)));
	formulae.(i) <- SHARP;
	nmessages := !nmessages+1;
      with Component_Of_Formula_Error -> print_endline ("No component for formula "^string_rep mou.(i));		
  done;
  (result,!nmessages,float_of_int (!smessages) /. float_of_int (Array.length formulae))
		 
let compute_obligations_simp (formulae: ltl array) (alpha:d_alphabet)= 
  let formulaeArray = Array.init (Array.length formulae) (fun i -> formulae.(i)) in 
  let result = Array.make (Array.length formulae) SHARP in 
  let mou = Array.init (Array.length formulae) (fun i -> most_urgent_obligation (formulaeArray.(i))) in
  for i=0 to (Array.length result) -1 do
    if (urgency (mou.(i)) >0) then
      try
	let index = component_of_formula mou.(i) alpha i in
	result.(index) <- simp (And (formulae.(i), result.(index)));
	formulae.(i) <- SHARP
      with Component_Of_Formula_Error -> print_endline ("No component for formula "^string_rep mou.(i));		
  done;
  result
		 
(* A function mem for arrays (same specification as the function List.mem for Lists)*)		 
let array_mem (elt:'a) (the_array:'a array) =
  let found = ref false and i = ref 0 in
  while (!i< Array.length the_array && not !found) do (
    if (the_array.(!i) = elt) then (
      found :=  true;
    );
    i := !i+1;
  )
  done;
  !found
			 
	
exception Monitor_EmptySequence

	
let compute_current_oblig (formulae: ltl array) (obligation:ltl array) =
  for i=0 to (Array.length formulae -1) do
    if (obligation.(i) <> SHARP) then
      formulae.(i) <- simp (And(formulae.(i), obligation.(i)))
  done;
  formulae

(* Progresses a formula with an event in a decentralized fashion. This
   function uses the local observation alphabet of component on which
   the progression is applied to.  It also uses: - the global trace (so
   as to possibly retrieving past events), - the current event read in
   the trace, and - the component index (to select the needed
   observation in the current event or a past event)
*)
let rec d_progress_form_old (formula:ltl) (e:event) (alpha:d_alphabet) (t:d_trace) (ed_number:int) (component:int)=
  let form = simp formula in
  match form with 
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
    | Or (f1,f2) -> (Or(d_progress_form_old f1 e alpha t ed_number component, d_progress_form_old f2 e alpha t ed_number component))
    | And(f1,f2) -> (And(d_progress_form_old f1 e alpha t ed_number component, d_progress_form_old f2 e alpha t ed_number component ))
    | Neg(f1) -> (Neg (d_progress_form_old f1 e alpha t ed_number component))
    | Next(f1) -> f1
    | Until (f1,f2) -> (Or(d_progress_form_old f2 e alpha t ed_number component, And(d_progress_form_old f1 e alpha t ed_number component, Until (f1,f2))))
    | Glob (f1) -> (And(d_progress_form_old f1 e alpha t ed_number component, Glob f1))
    | Ev(f1) -> (Or (d_progress_form_old f1 e alpha t ed_number component, Ev f1))
    | Xor (f1,f2) -> (Xor(d_progress_form_old f1 e alpha t ed_number component, d_progress_form_old f2 e alpha t ed_number component))

    | _ -> raise (DProgress_Error(form,e,alpha,t,ed_number))
 
    
(*
 Performs the decentralized progression of an array of formulae. The array of formulae represents the set of obligations currently monitored in the system. 
 This function simply invokes on each formula in the array the decentralized progression that applies to one formula.
*)
let d_progress_old (formulae:ltl array) (ed:d_event) (alpha:d_alphabet) (t:d_trace) (ed_number:int) =
  for i=0 to (Array.length formulae-1) do
    let edi = List.nth ed i in
    nb_progressions := !nb_progressions + count_needed_progressions formulae.(i);
    try
      formulae.(i) <- simp (d_progress_form_old formulae.(i) edi (alpha:d_alphabet) (t:d_trace) (ed_number:int) i)
    with DProgress_Error (form,e,alpha,t,ed_number) -> (
      print_endline("Error when progressing "^string_rep form);
      print_endline("\t with event "^stringrep_event e);
      print_endline("\t and alphabet "^stringrep_alphabet (globalAlphabet alpha));
    )
  done;
  formulae




(*
The main decentralised monitoring function. It is a recursive function that is processing the remaining trace to read.
It is an offline monitoring function performing trace analysis in a forward direction. This function simulates what is going on monitors.
Each monitor conjuncts the received obligations to its current obligation. Then, depending on what remains to be read it progresses accordingly. 

*)
let rec d_monitor_old (formulae:ltl array) (alpha:d_alphabet) (t:d_trace) (ed_number:int) (obligations:ltl array) (toRead:d_trace) (readSoFar:d_trace) (nmess:int) (smess:float) =

  if (ed_number >= List.length t) then
    (Undeter, readSoFar,nmess,smess /. float_of_int (length readSoFar), !nb_progressions)
  else (
    let formulae = compute_current_oblig formulae obligations in 
    if  (array_mem True formulae) then
      (True,readSoFar,nmess,smess /. float_of_int (length readSoFar), !nb_progressions)
    else (
      if (array_mem False formulae) then
	(False,readSoFar,nmess,smess /. float_of_int (length readSoFar), !nb_progressions)
      else
	match toRead with
	    [] -> raise Monitor_EmptySequence
	  | (ed:d_event)::[] ->
	    (let formulae = (d_progress_old formulae ed alpha t (ed_number)) in
	     if (array_mem True formulae) then
	       (True,readSoFar@[ed],nmess,smess /. float_of_int (length readSoFar + 1), !nb_progressions)
	     else
	       (
		 if (array_mem False formulae) then
		   (False,readSoFar@[ed],nmess,smess /. float_of_int (length readSoFar + 1), !nb_progressions)
		 else
		   (Undeter,readSoFar@[ed],nmess,smess /. float_of_int (length readSoFar + 1), !nb_progressions)
	       )
	       
	    )
	  | (ed:d_event)::(remainder:d_trace) ->
	    (let formulae = Array.map simp (d_progress_old formulae ed alpha t (ed_number)) in
	     if (array_mem True formulae) then
	       (True,readSoFar@[ed],nmess,smess /. float_of_int (length readSoFar + 1), !nb_progressions)
	     else
	       (
		 if (array_mem False formulae) then
		   (False,readSoFar@[ed],nmess,smess /. float_of_int (length readSoFar + 1), !nb_progressions)
		 else (
		   let (next_obligations,n,s) = compute_obligations formulae alpha in
		   d_monitor_old formulae alpha t (ed_number+1) (Array.map simp next_obligations) remainder (readSoFar@[ed]) (nmess+n) (smess+.s)
		 )
	       )
	    )
    )
  )

    

let md_monitor (formulae:ltl array) (alpha:d_alphabet) (t:d_trace) (ed_number:int) (obligations:ltl array) (toRead:d_trace) (readSoFar:d_trace) (nmess:int) (smess:float) =
  
  let trace_length = List.length t in
  
  let rec d_monitor_rec (formulae:ltl array) (ed_number:int) (obligations:ltl array) (toRead:d_trace) (readSoFar:d_trace) (nmess:int) (smess:float) =
    if (ed_number >= trace_length) then
      (Undeter, readSoFar, nmess, smess /. float_of_int (length readSoFar), !nb_progressions)
    else (
      let formulae = compute_current_oblig formulae obligations in 
      if  (array_mem True formulae) then
	(True,readSoFar,nmess, smess /. float_of_int (length readSoFar), !nb_progressions)
      else (
	if (array_mem False formulae) then
	  (False,readSoFar,nmess,smess /. float_of_int (length readSoFar), !nb_progressions)
	else
	  match toRead with
	      [] -> raise Monitor_EmptySequence
	    | (ed:d_event)::[] ->
	      (let formulae = (md_progress formulae ed alpha t (ed_number)) in
	       if (array_mem True formulae) then
		 (True,readSoFar@[ed],nmess, smess /. (float_of_int (length readSoFar + 1)), !nb_progressions)
	       else
		 (
		   if (array_mem False formulae) then
		     (False,readSoFar@[ed],nmess,smess /. (float_of_int (length readSoFar + 1)), !nb_progressions)
		   else
		     (Undeter,readSoFar@[ed],nmess,smess /. (float_of_int (length readSoFar + 1)), !nb_progressions)
		 )
		   
	      )
	    | (ed:d_event)::(remainder:d_trace) ->
	      (
		let formulae = (md_progress formulae ed alpha t (ed_number)) in
		if (array_mem True formulae) then
		  (True,readSoFar@[ed],nmess, smess /. float_of_int (length readSoFar + 1), !nb_progressions)
		else
		  (
		    if (array_mem False formulae) then
		      (False,readSoFar@[ed],nmess,smess /. float_of_int (length readSoFar + 1), !nb_progressions)
		    else
		      (
			let (next_obligations,n,s) = compute_obligations formulae alpha in
			d_monitor_old formulae alpha t (ed_number+1) (Array.map simp next_obligations) remainder (readSoFar@[ed]) (nmess+n) (smess +. s)
		      )
		  )
	      )
      )
    )
      
  in
  nb_progressions := 0;
  d_monitor_rec formulae 0 obligations toRead [] 0 0.

  (**
let rec d_monitor_display_obligs (formulae:ltl array) (alpha:d_alphabet) (t:d_trace) (ed_number:int) (obligations:ltl array) (toRead:d_trace) (readSoFar:d_trace) (nmess:int) (smess:int)=
  
  Printf.printf "################\n t= %i\n################\nFormulae:\n'%s'\n###\nObligations:\n'%s'\n###\nEvent:\n'%s'\n#####\n" (List.length readSoFar) (string_rep_input_formulae formulae) (string_rep_input_formulae obligations) (string_rep_devent (List.nth t ed_number)) ;

  if (ed_number >= List.length t) then (False,readSoFar,nmess,smess)
  else (
    let formulae = compute_current_oblig formulae obligations in 
    if  (array_mem True formulae)
    then (True,readSoFar,nmess,smess)
    else (
      if (array_mem False formulae) then (False,readSoFar,nmess,smess)
      else
	match toRead with
	    [] -> raise Monitor_EmptySequence
	  | (ed:d_event)::[] ->
	    (let formulae = Array.map simp (d_progress formulae ed alpha t (ed_number)) in
	     if (array_mem True formulae)
	     then (True,readSoFar@[ed],nmess,smess)
	     else (False,readSoFar@[ed],nmess,smess)
	       
	    )
	  | (ed:d_event)::(remainder:d_trace) ->
	    (let formulae = Array.map simp (d_progress formulae ed alpha t (ed_number)) in
	     if (array_mem True formulae) then (True,readSoFar@[ed],nmess,smess)
	     else (
	       if (array_mem False formulae) then (False,readSoFar@[ed],nmess,smess)
	       else (
		 let (next_obligations,n,s) = compute_obligations formulae alpha in
		 d_monitor_display_obligs formulae alpha t (ed_number+1) (Array.map simp next_obligations) remainder (readSoFar@[ed]) (nmess+n) (smess+s))
	     )
	    )
    )
  )
			
  **)
