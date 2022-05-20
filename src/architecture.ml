open Utils
open Alphabetevent
open Dltl

(**
Architecture
**)

module Int = struct type t = int let compare = compare end
module Architecture = Map.Make(Int)
type architecture = (string list) Architecture.t
module ArchiMap =  Map.Make (String)
type archimap = int ArchiMap.t

let empty_architecture = Architecture.empty

let stringrep_architecture (archi:architecture):string =
  let string_of_one_comp (c:string list):string =
    let content = List.fold_left (fun acc elt -> if acc = "" then elt else acc^"; "^elt) "" c
    in "["^content^"]"
  in
  Architecture.fold (fun k elt acc -> let nacc = if acc = "" then acc else acc^"\n" in nacc^(string_of_int k)^" -> "^(string_of_one_comp elt)) archi ""
  
let empty_archimap = ArchiMap.empty

let build_archi_map (archi:architecture): archimap =
  let result = ArchiMap.empty in
  let add_props (comp_number:int) (l:string list) (archimap: archimap): archimap =
    List.fold_left (fun am elt -> ArchiMap.add elt comp_number am) archimap l
  in
  Architecture.fold add_props archi result

(**
score i phi archi is the score of formula phi on component i given architecture archi
**)

let score (i:int) (phi:dltl) (ar:architecture):int=
  let archi = build_archi_map ar in
  let rec score_r (phi:dltl):int =
    match phi with
      | Var (v) ->
	if (ArchiMap.mem v archi) then
	  let id = ArchiMap.find v archi in if id = i then 1 else 0
	else
	  0
      | Neg (phip) | Next (phip) | Glob (phip) | Ev (phip) -> score_r phip
      | Or (phi1, phi2) | And (phi1,phi2) | Imp (phi1,phi2) | Iff (phi1,phi2) | Until (phi1,phi2) | Wuntil (phi1,phi2) | Xor (phi1,phi2)
	  -> score_r phi1 + score_r phi2
  | _ -> 0
  in
  score_r phi

    
let chc (phi:dltl) (ar:architecture) (default:int) : int =
  let ar_bindings = Architecture.bindings ar in
  let components = List.map (fun (k,_) -> k) ar_bindings in
  let component_scores = List.map (fun c -> (c,score c phi ar)) components in
  let maxscore = Utils.max_list (List.map (fun (_,score) -> score) component_scores) in
  if (maxscore = 0)
  then
    default
  else
    let (choice_comp,_) = List.find (fun (_,score) -> score = maxscore) component_scores in
    choice_comp

(** ///////////////////////////////////////////////////
    Conversion between architecture and d_alphabet
    //////////////////////////////////////// **)

let architecture_2_dalphabet (archi:architecture) : d_alphabet =
  Architecture.fold (fun k elt acc -> acc@([elt])) archi []

let dalphabet_2_architecture (dalpha:d_alphabet) : architecture =
  let (archi,_) =
    List.fold_left (fun (m,cpt) elt -> let newm = Architecture.add cpt elt m in (newm, cpt+1)) (Architecture.empty,0) dalpha in
  archi
