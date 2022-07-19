(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com>, Ylies Falcone <ylies.falcone@gmail.com> 
   Code is released under the GPL license version 3. *)
   open List

type alphabet = string list
type event = string list
type trace = event list

type d_alphabet = alphabet list;;
type d_event = event list;;
type d_trace = d_event list;;

exception Error

(* Computes the global alphabet corresponding to a distributed alphabet *)
let globalAlphabet (alpha:d_alphabet) = concat alpha;;

let string_rep_gen (e:string list) (separator:string) (s_begin:string) (s_end:string) =
	if ((length e)=0) then s_begin^" "^s_end
			else s_begin^(fold_left (fun x y -> if (x="") then y
										else x^separator^y) "" e)^s_end

(* String representations of an event *)
let string_rep_event (e:event) = string_rep_gen e "," "{" "}"
let string_rep_eventbis (e:event) = string_rep_gen e "," "" ""
										
(* String representations of a trace *)
let string_rep_trace (t:trace) = 
	string_rep_gen (map string_rep_event t) " ; " "" ""	

(* String representations of a decentralized event *)
let string_rep_devent (e:d_event) =
	"{"^fold_left (fun x y -> if x="" then (if y="" then "" else y) else x^"|"^y) "" (map string_rep_eventbis e)^"}"

(* String representations of an alphabet *)
let string_rep_alphabet (alpha:alphabet) = string_rep_event alpha

(* String representations of a decentralized alphabet *)
let string_rep_dalphabet (alpha:d_alphabet) = string_rep_devent alpha

(* String representations of a decentralized trace *)
let string_rep_dtrace (t:d_trace) =
	string_rep_gen (map string_rep_devent t) " ; " "" ""

(* The global trace corresponding to a dencetralized trace *)
let rec globalTrace (t:d_trace) =
	match t with
		[] -> []
		| de::[] -> [concat de]
		| de::remainder -> [concat de] @ globalTrace remainder
		
		
(* Conversion from alphabet to dalphabets *)
let rec choose k l =
  if k = 0 
  then [ [] ]
  else
    let len = List.length l in
    if len < k
    then []
    else if k = len
    then [ l ]
    else
      match l with
      h :: t ->
          let starting_with_h =
            (List.map (fun sublist -> h :: sublist) (choose (pred k) t))
          in
          let not_starting_with_h = choose k t in
          starting_with_h @ not_starting_with_h
      | [] -> assert false

(* An auxiliary function to replace an element in a list at a given index *)
let rec replace_at (the_list:'a list) (index:int) (elt:'a) =
	match the_list with
		[] -> []
		| e::remainder -> if (index=0) then elt::remainder
							else e::(replace_at remainder (index-1) elt)
								
(* Add randomly an element in a decentralized alphabet *)
let add_somewhere (dalpha:d_alphabet) elt =
	let index = Random.int (List.length dalpha) in
	let new_elt = (List.nth dalpha index) @ elt in
		replace_at dalpha index new_elt
		
(* Add an element to every subalphabet of a given decentralized alphabet *)
let add_everywhere (dalpha:d_alphabet) elt = 
	let result = ref [] in (
	for i=0 to (List.length dalpha) -1 do
		result := !result @ [(replace_at dalpha i ((List.nth dalpha i)@elt))];
	done;
	!result
	)

(* Transforms a (global) alphabet into a decentralized one by considering that each symbol of the alphabet goes to a different sub-alphabet in the decentralized one *)
let rec transform_into_dalphabet (alpha:alphabet) =
	match alpha with
		[] -> []
		| e::[] -> [[e]]
		| e::remainder -> [[e]]@transform_into_dalphabet(remainder)

(* Completes a decentralized aphabet with the symbols in a global alphabet.
Selects only the elements of the global alphabet which are not in the decentralized one *)
let complete_alpha (alpha:alphabet) (dalpha:d_alphabet) =
	let dgalpha	= globalAlphabet dalpha in
	let non_placed_elts = List.filter (fun x -> not(List.mem x dgalpha)) alpha in
	add_everywhere dalpha non_placed_elts
	
(* Generates all decentralized alphabet (of various sizes) that are compatible with the global alphabet *)
let generate_compatible_dalphabet (alpha:alphabet) (size:int) =
	if (List.length alpha<size) then []
	else (
		let base_alphabets = choose size alpha in
		let base_dalphabets = List.map transform_into_dalphabet base_alphabets in
			List.fold_left (fun x y -> x@(complete_alpha (alpha) y)) [] base_dalphabets
		

	)
	
	
	