open Dltl
open Batteries
open DynArray
open Common_network
open Network

module TimeInstant = struct type t = int let compare = compare end
type time_instant = TimeInstant.t
module Cell = Map.Make(TimeInstant)
type cell = dltl Cell.t
type cell_index = int
type dyn_net_loc = cell array
type loc_index = int
type dyn_net = dyn_net_loc array
    
let get_location (n:dyn_net) (lindex:loc_index) : dyn_net_loc =
  n.(lindex)

let get_cell (n:dyn_net) (lindex:loc_index) (cindex:cell_index) : cell =
 (n.(lindex)).(cindex)

let get_formula (n:dyn_net) (lindex:loc_index) (cindex:cell_index) (t:time_instant) : dltl =
   Cell.find t (n.(lindex)).(cindex)

(** ///////////////////////////////////////////////////////////
    String of dyn_net
    ///////////////////////////////////////////////////////////**)

let stringrep_timeinstant (t:time_instant) : string = string_of_int t

let string_rep_cell (c:cell) : string =
  let content = Cell.fold (fun k elt acc -> acc^(if acc = "" then "" else ",")^(string_of_int k)^" -> "^(string_rep elt)) c "" in
  "["^content^"]"

let stringrep_loc (loc:dyn_net_loc) : string =
  let content = Array.fold_left (fun acc elt -> acc^(if acc = "" then "" else ";")^(string_rep_cell elt)) "" loc in
  "["^content^"]"

let stringrep_dyn_net (n:dyn_net) : string =
  Array.fold_left (fun acc elt -> acc^(if acc = "" then "" else "\n")^(stringrep_loc elt)) "" n
  
(** ///////////////////////////////////////////////////////////
    Conversion from a network to a dyn_net
    /////////////////////////////////////////////////////////// **)

let convert_dltl_for_dyn_network (phi:dltl) : dltl =
  let rec conv = function
    | Neg (f) -> Neg (conv f)
    | Next (f) ->  Next (conv f)
    | Glob (f) ->  Glob (conv f)
    | Ev (f) ->  Ev (conv f)
    | Or (f1, f2) -> Or (conv f1, conv f2)
    | And (f1, f2) -> And (conv f1, conv f2)
    | Iff (f1, f2) -> Iff (conv f1, conv f2)
    | Imp (f1, f2) -> Imp (conv f1, conv f2)
    | Until (f1, f2) -> Until (conv f1, conv f2)
    | Wuntil (f1, f2) -> Wuntil  (conv f1, conv f2)
    | Xor (f1, f2) -> Xor (conv f1, conv f2)             
    | f -> f in
  conv phi
    
let make_cell (f:dltl) : cell =
  Cell.add 0 f Cell.empty

let make_dyn_net_loc (nl:network_location) : dyn_net_loc =
  to_array (DynArray.map (fun elt -> (make_cell (convert_dltl_for_dyn_network elt))) nl)
 
let make_dyn_net (n:network) : dyn_net =
  let length = Network.cardinal n in
  let empty_loc : dyn_net_loc = Array.make 1 (Cell.empty) in 
  let empty_net : dyn_net = Array.make length empty_loc in
  let _ = Network.mapi (fun k elt -> Array.set empty_net k (make_dyn_net_loc elt)) n in
  empty_net

    
