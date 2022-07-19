open List


let max (a: int) (b: int): int  =
if a > b then a else b
  
let round_generic (f:float) (precision : int) : float =
  let candidate = (floor (f *. (10. ** float_of_int precision))) /. (10. ** float_of_int precision) in
  if candidate > 10000. then
    floor (candidate)
  else
    (
      if (candidate >= 1000.) then
        floor (candidate *. 100.) /. 100.
      else
        candidate
    )

let round_var (f:float) : float = round_generic f 2


let variance (sum_square:float) (avg:float) (size:int) =
  round_var ( ((sum_square /. (float_of_int size)) -. (avg**2.))**(1./.2.))


  (** ///////////////////////////////////////////////////////////
    Some extra functions for List
    /////////////////////////////////////////////////////////// **)
  
let rec max_list (l:int list):int =
  match l with
      [] -> failwith "no element in the list"
    | [e] -> e
    | e::lp -> let mlp = max_list lp in if e > mlp then e else mlp

let rec min_list (l:int list):int =
  match l with
      [] -> failwith "no element in the list"
    | [e] -> e
    | e::lp -> let mlp = min_list lp in if e < mlp then e else mlp

let rec equal_list (l1:'a list) (l2: 'a list):bool =
    match l1, l2 with
	[], [] -> true
      | e1::r1, e2::r2 -> e1 = e2 && equal_list r1 r2
      | _ -> false

let rec sum_int_list (l: int list) =
  match l with
      hd::tl -> hd + (sum_int_list tl)
    | [] -> 0

let rec sum_float_list (l: float list) =
  match l with
      hd::tl -> hd +. (sum_float_list tl)
    | _ -> 0.

let rec sum_squared_int_list (l: int list) =
  match l with
      hd::tl -> (hd * hd) + (sum_squared_int_list tl)
    | _ -> 0

let rec sum_squared_float_list (l: float list) = 
  match l with
      hd::tl -> (hd ** 2.) +. (sum_squared_float_list tl)
    | _ -> 0.

(* Divides the element of l1 by those of l2, assumes both lists are of the same size *)
let rec div_list (l1: float list) (l2: float list) =
  match l1, l2 with
      0.0::tl1, 0.0::tl2 -> 0.0::(div_list tl1 tl2)
    | hd1::tl1, hd2::tl2 -> (hd1 /. hd2)::(div_list tl1 tl2)
    | _, _ -> []

(* Returns the number of different element between both lists. If they do not have the same size, then the elements of the
  smaller list are compared to the corresponding ones in the larger list and the remaining element of the latter are not
  compared to anything *)
let rec nb_diff_list (l1: 'a list) (l2: 'a list) =
  match l1, l2 with
      [], _ | _, [] -> 0
    | hd1::tl1, hd2::tl2 -> if (mem hd1 l2) && (mem hd2 l1) then nb_diff_list tl1 tl2 else 1 + nb_diff_list tl1 tl2

let make_list (size:int) (fill_function:int -> 'a):'a list =
  let rec list_of_indexes n = if n=1 then [1] else (list_of_indexes (n-1))@[n] in
		map fill_function (list_of_indexes size)
		
let map3 (f:'a -> 'b -> 'c -> 'd) (a:'a list) (b:'b list) (c:'c list) =
	map2 (fun x (y1,y2) -> f x y1 y2) a (map2 (fun x y -> (x,y)) b c)

let iter3 (f:'a -> 'b -> 'c -> unit) (a:'a list) (b:'b list) (c:'c list) =
	iter2 (fun x (y1,y2) -> f x y1 y2) a (map2 (fun x y -> (x,y)) b c)


let rec prefix (n:int) (l:'a list) =
	let rec prefix_rec (n:int) (l:'a list) (r:'a list) =
		if (n=0) then r
		else
			match l with
				[] -> failwith "error argument l"
				| head::remainder -> 
			prefix_rec (n-1) remainder (r @ [head])
	in 
		prefix_rec n l []

let rec suppress_duplicates (l:'a list) : 'a list =
  match l with
    | [] -> []
    | [e] -> [e]
    | e::rem -> let remnodupes = suppress_duplicates rem in
		(if (mem e rem) then [] else [e])@remnodupes  
		  
(** //////////////////////////////////////////////////////////////////
    Shared string representation of some data structure
    ////////////////////////////////////////////////////////////// **)
let stringrep_intint ((x,y): int*int) : string =
  "("^(string_of_int x)^","^(string_of_int y)^")"

open Printf

let stringrep_list (srep_element: 'a -> string) (l: 'a list) : string =
  let body = List.fold_left (fun acc elt -> acc^(if acc = "" then "" else ",")^(srep_element elt)) "" l in
  "["^body^"]"
    
let stringrep_intint_list (l:(int*int) list): string =
 stringrep_list stringrep_intint l
    
let stringrep_int_list (l:int list): string =
  stringrep_list string_of_int l


let stringrep_array (a: 'a array) (srep_element: 'a -> string) (return:bool) : string =
  let size = Array.length a in
  let possible_return = (if return then "\n" else "") in
  let rec sr (index:int) : string =
    if (index > size - 1) then
      ""
    else
      (
	let string_of_element = (if index <> 0 then possible_return else "")^(string_of_int index)^":"^" "^(srep_element a.(index)) in
	(if (index <> 0 && not return) then ", " else "")^string_of_element^(sr (index + 1))
      )
  in
  "["^possible_return^(sr 0)^possible_return^"]"

let stringrep_arrayarray (a: 'a array array) (srep_element: 'a -> string) : string =
  stringrep_array a (fun x -> stringrep_array x srep_element false) true

(**
Writes to a file (append mode)
Does not make any assumption on the file.
**)
let write_to_file (file_name:string) (content:string) =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 file_name in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" content;   (* write something *)
  close_out oc; (* flush and close the channel *)
    

	
