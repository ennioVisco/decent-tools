open List

type alphabet = string list
type event = string list

type d_alphabet = alphabet list
type d_event = event list

exception Error


let event2devent (e:event) (dalpha:d_alphabet) : d_event =
  List.map (fun elt -> filter (fun p -> mem p e) elt) dalpha

(* Computes the global alphabet corresponding to a distributed alphabet *)
let globalAlphabet (alpha:d_alphabet) = concat alpha;;

(* Tests whether two d_event are equal regardless of the ordering of atomic proposition in them *)
let test_devent_equality (e1: d_event) (e2: d_event) =
  let rec event_included (e1: event) (e2: event) =
    match e1 with
        hd::tl -> if (mem hd e2) then event_included tl e2 else false
      | _ -> true
    
  in
  let rec test_devent_equality_rec (e1: d_event) (e2: d_event) =
    match e1, e2 with
        hd1::tl1, hd2::tl2 -> if (event_included hd1 hd2) && (event_included hd2 hd1) then test_devent_equality_rec tl1 tl2 else false 
      | _ -> true
  
  in test_devent_equality_rec e1 e2

let compare_event (e1: event) (e2: event) =
  let e1_ordered = List.sort String.compare e1 in
  let e2_ordered = List.sort String.compare e2 in
  
  let rec order_event_rec (e1: event) (e2: event) =
    match e1, e2 with
        [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | hd1::tl1, hd2::tl2 -> (
          let cmp_hd = String.compare hd1 hd2 in
          if cmp_hd = 0 then
            order_event_rec tl1 tl2
          else if cmp_hd > 0 then
            1
          else
            -1
        )
 
  in order_event_rec e1_ordered e2_ordered


let string_rep_gen (e:string list) (separator:string) (s_begin:string) (s_end:string) =
	if ((length e)=0) then s_begin^" "^s_end
			else s_begin^(fold_left (fun x y -> if (x="") then y
										else x^separator^y) "" e)^s_end

(* String representations of an event *)
let stringrep_event (e:event) = string_rep_gen e "," "{" "}"
let stringrep_eventbis (e:event) = string_rep_gen e "," "" ""
	

(* String representations of a decentralized event *)
let stringrep_devent (e:d_event) =
	"{"^fold_left (fun x y -> if x="" then (if y="" then "" else y) else x^"|"^y) "" (map stringrep_eventbis e)^"}"

(* String representations of an alphabet *)
let stringrep_alphabet (alpha:alphabet) = stringrep_event alpha

(* String representations of a decentralized alphabet *)
let stringrep_dalphabet (alpha:d_alphabet) = stringrep_devent alpha

    
	
	
let are_two_alphabets_eq (alpha1:alphabet) (alpha2:alphabet) : bool =
  List.for_all (fun elt1 -> List.exists (fun elt2 -> elt1 = elt2) alpha2) alpha1
  && List.for_all (fun elt2 -> List.exists (fun elt1 -> elt1 = elt2) alpha1) alpha2

let are_two_dalphabet_eq (dalpha1:d_alphabet) (dalpha2:d_alphabet) : bool =
  List.for_all (fun elt1 -> List.exists (fun elt2 -> are_two_alphabets_eq elt1 elt2) dalpha2) dalpha1
  && List.for_all (fun elt2 -> List.exists (fun elt1 -> are_two_alphabets_eq elt2 elt1) dalpha1) dalpha2

		
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

let list_partition (l: 'a list) =

  let rec add_elem_inside (e: 'a) (l: 'a list list) =
    match l with
        hd::tl -> [[e::hd] @ tl] @ (List.map (fun lst -> hd::lst) (add_elem_inside e tl))
      | _ -> []

  in let rec add_elem (e: 'a) (l: 'a list list list) =
    match l with
        hd::tl -> [[e] :: hd] @ (add_elem_inside e hd) @ (add_elem e tl)
      | _ -> []

  in let rec list_partition_rec (l: 'a list) =
    match l with
        hd::tl -> (
          let smaller_partition = list_partition_rec tl in
          match smaller_partition with
              [] -> [[[hd]]]
            | hd_p::tl_p -> add_elem hd smaller_partition
        )
      | _ -> []

  in list_partition_rec l

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
    let candidates = 
    let base_alphabets = choose size alpha in
    let base_dalphabets = List.map transform_into_dalphabet base_alphabets in
    List.fold_left (fun x y -> x@(complete_alpha (alpha) y)) [] base_dalphabets in
    let rec filter l =
      match l with
	  [] -> []
	| elt1::rem -> if (List.exists (fun elt2 -> are_two_dalphabet_eq elt1 elt2) rem) then
	    filter rem
	  else
	    elt1::(filter rem)
    in
    filter candidates
  )

let generate_all_compatible_dalphabets (alpha:alphabet) : d_alphabet list =
  let rec gen (n:int) : d_alphabet list =
    if n = 0 then []
    else
      generate_compatible_dalphabet alpha n @ (gen (n-1))
  in
  gen (length alpha)
