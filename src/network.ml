open Utils
open Batteries
open Dltl
open DynArray
open Architecture
open Common_network

(** ///////////////////////////////////////////////////////////////
    Networks
    ///////////////////////////////////////////////////////////////**)
  
type network_location = dltl DynArray.t
module ComponentId = struct type t = int let compare = compare end
module Network = Map.Make(ComponentId)
type network = network_location Network.t

(** equality between network locations **)
let equal_network_location (l1:network_location) (l2:network_location): bool =
  let ll1 = to_list l1 and ll2 = to_list l2 in
  equal_list ll1 ll2

(** Conversion to string **)
let stringrep_network_location (nl:network_location) : string =
  let content = fold_left (fun acc elt -> let elts = string_rep elt in if acc = "" then elts else acc^"; "^elts) "" nl
  in "[[ "^content^" ]]"
    
let stringrep_network (n:network) : string =
  Network.fold (fun k v acc -> let cs = stringrep_network_location v and ks = string_of_int k in let elts = ks^" -> "^cs in if acc = "" then elts else acc^"\n"^elts) n ""


(** Creates an empty network **)
let make_empty_network (n:int): network =
  let rec make_empty_network_rec (n:int) (net:network) =
  let empty_dynarray:network_location = DynArray.create () in
    if n = -1
    then net
    else
      make_empty_network_rec (n-1) (Network.add n empty_dynarray net)
  in
  make_empty_network_rec (n-1)  Network.empty
;;

(** Computes a static network given an architecture and an LTL formula
    following the definition in paper **)
let network (archi: architecture) (i:int) (n:network) (phi:dltl): network =
  let rec net (i:int) (n:network) (phi:dltl): network =
  (**  print_endline ("##############\nnet "^string_of_int i^"\n"^stringrep_network n^"\n "^string_rep phi^"\n#######");**)
    let np, phip = distr i n phi in
    let npi : network_location = Network.find i np in
    add npi phip;
    Network.add i npi np
      
  and
      distr (i:int) (n:network) (phi:dltl): network * dltl =
(**     print_endline ("##############\ndistr "^string_of_int i^"\n"^stringrep_network n^"\n "^string_rep phi^"\n#######");**)
    match phi with
      | Neg (psi) | Next (psi) | Glob (psi) | Ev (psi) ->
	let np, psip = distr i n psi in
	let psipp = match phi with
	  | Neg (_) -> Neg (psip)
	  | Next (_) -> Next (psip)
	  | Glob (_) -> Glob (psip)
	  | Ev (_) -> Ev (psip)
	  | _ -> failwith "this should not happen"
	in np, psipp
      | And (phi1, phi2) | Or (phi1, phi2) | Iff (phi1, phi2) | Imp (phi1, phi2) | Until (phi1, phi2) | Wuntil (phi1, phi2) | Xor (phi1, phi2) ->
	let n1, phi1p = recurs i n phi1 in
	let n2, phi2p = recurs i n1 phi2 in
	n2, (
	  match phi with
	    | And (_, _) -> And (phi1p, phi2p)
	    | Or (_, _) -> Or (phi1p, phi2p)
	    | Iff (_, _) -> Iff (phi1p, phi2p)
	    | Imp (_, _) -> Imp (phi1p, phi2p)
	    | Until (_, _) -> Until (phi1p, phi2p)
	    | Wuntil (_, _) -> Wuntil (phi1p, phi2p)
	    | Xor (_, _) -> Xor (phi1p, phi2p)
	    | _ -> failwith "this should not happen"
	)
      | _ -> n,phi
  and
      recurs (i:int) (n:network) (phi:dltl): network * dltl =
    let c = chc phi archi i in
    if (i=c) then
      distr i n phi
    else
      let new_n = net c n phi
      and new_form = Dists (c, (length (Network.find c n))- 1, phi) in
      (new_n, new_form)
  in net i n phi

let main_formula (archi: architecture) (n:network) (f:dltl): dltl =
  let c = chc f archi 0
  in
  let compo = Network.find c n
  in
  let l = length compo
  in
  get compo (l-1)

let depth_network (n:network) (archi:architecture) (f:dltl) : int =
  let mf = main_formula archi n f in
  let rec cd (phi:dltl) : int =
    match phi with
      | Or (f1, f2) | And (f1, f2) | Iff (f1, f2) | Xor (f1, f2) | Imp (f1, f2) | Until (f1, f2) | Wuntil (f1, f2) -> (let cf1 = cd f1 and cf2 = cd f2 in if cf1 > cf2 then cf1 else cf2)
      | Neg f | Next f | Glob f | Ev f -> cd f
      | Dists (i,j,_) | Distd (i, j, _) -> 1 + cd (get (Network.find i n) j)
      | True | False | Var _ | SHARP  -> 0
  in 1 + cd mf


(** /////////////////////////////////////////////////////////////////
    Compacting the network
    ///////////////////////////////////////////////////////**)
  
let detect_duplicates_at_location (l:network_location) : dltl list =
  let llist = to_list l in
  let rec dd l =
    match l with
      | [] -> []
      | f::rem -> let lwith = List.filter (fun elt -> elt = f) rem
      and lwithout = List.filter (fun elt -> elt <> f) rem in
		  if (List.length lwith <> 0) then f::(dd lwithout)
		  else dd rem
  in dd llist

let indexes_of_form (f:dltl) (l:network_location): int list =
  let len = length l in
  let rec indf i = if i > (len - 1) then [] else (if (get l i = f) then i::(indf (i+1)) else (indf (i+1)))
  in let indexes = indf 0 in
     indexes

let substitute_network (indexes:(int*int) list) (n:network) (i:int) (j:int) : network =
  let substitute_one_comp (l:network_location) : network_location =
    map (fun elt -> substitute_pointer_by_pointer_list elt indexes i j) l in
  Network.map (fun elt -> substitute_one_comp elt) n

(** Applies a transformation to the network based on the duplicates of
    a component; It takes as input the index of the currently working
    network location, the network location, and the current value of the
    network.  For each the network_locaiton, it computes the sert of
    duplicate formulae and their indexes.  From the set of indexes, it
    computes the minimal index.  In the current network location, all
    duplicates are suppressed but the one of minimal index.  In other
    network location, all pointers to a duplicate are changed to a pointer
    to the formula with the minimal index.  **)
let compact_one_component (idx:int) (l:network_location) (n:network) : network=
  let dups:dltl list = detect_duplicates_at_location l in
  let indexesform: ((dltl*((int*int) list)*int) list) =
    List.map (fun f -> let indexes:int list = indexes_of_form f l in
		       let minindexes:int = Utils.min_list indexes in
		       (f,List.map (fun elt -> (idx,elt)) indexes,minindexes)) dups in
  let net_changedref = List.fold_left (fun acc (_,indexes, minindexes) -> substitute_network indexes acc idx minindexes) n indexesform in
  let suppress_forms (cidx:int) (indexes: int list) : unit =
    List.iter (fun i -> set (Network.find idx net_changedref) i SHARP) indexes in
  List.iter (fun f -> let index_suppress = match (indexes_of_form f l) with [] | [_] -> failwith "list of indexes should have at least 2 elements" | _::r -> r  in suppress_forms idx (index_suppress)) dups;
  let result = net_changedref in     
  result

(** Compacts the network. It applies a transformation to the
    network until it stabilizes.  The transformation applies to each
    component in the network.  **)
let compact (n:network): network =
  let stabilize_one_step (n:network) : network =
    Network.fold (fun k elt a -> compact_one_component k elt a) n n in
  let rec stabilize_rec (cur:network) (old:network):network =
    if Network.equal equal_network_location old cur then cur
    else (stabilize_rec (stabilize_one_step cur) cur) in
  stabilize_rec n Network.empty

(** Decrements the pointers to a component in a formula.  It takes as
    input the index of the network_location of interest and the
    formula.  It scans the formula, each time it encouters a network,
    it decrements its second coordinate if the first coordinate is
    equal to the coordinate given in parameter. It leaves the other
    subformulae unchanged.  **)
let rec decrement_pointers (i:int) (j:int) (f:dltl):dltl =
  let rec dp f =
  match f with
  | Or (fp, fpp) -> Or (dp fp, dp fpp)
  | And (fp, fpp) -> And (dp fp, dp fpp)
  | Neg fp -> Neg (dp fp)
  | Iff (fp, fpp)  -> Iff (dp fp, dp fpp)
  | Imp (fp, fpp)  -> Imp (dp fp, dp fpp)
  | Until (fp, fpp) -> Until (dp fp, dp fpp)
  | Wuntil  (fp, fpp) -> Wuntil (dp fp, dp fpp)
  | Next fp -> Next (dp fp)
  | Glob fp -> Glob (dp fp)
  | Ev fp -> Ev (dp fp)
  | Xor (fp, fpp) -> Xor (dp fp, dp fpp)
  | Dists (x,y,fp) when x=i && y>=j-> Dists (x, y - 1, fp)
  | f -> f
  in dp f

(** Decrements the pointers appearing in the formulae in a network
    location after some index **)
let decrement_pointers_after_index (l:network_location) (cindex:int) (index:int):network_location =
  map (fun elt -> decrement_pointers cindex index elt) l

    
(** Remove SHARP symboles from a network location.  (SHARP symbols
    are introduced during network compacting: SHARPs replace the
    suppressed duplicates.)  The function simply keeps the formuale
    different from SHARP.  **)
let remove_sharps (l:network_location):network_location =
  filter (fun elt -> elt <> SHARP) l     

(** In a network location, the function retrieves the indexes of
    formulae that are SHARP.  It sequentially scans the network
    location and build a list by adding the current index whenever the
    formula is a sharp.  **)
let get_sharp_indexes (l:network_location) : int list =
  let lengthl = length l in
  let rec gsi i =
    if i > (lengthl - 1) then
      []
    else
      (
	let rem = gsi (i+1) in
	if get l i = SHARP then
	  i::rem
	else
	  rem
      ) in
  gsi 0

(** Reindexing of a network.  It uses internally function
    reindex_for_one_location which is applied to every element in the
    network.  Function reindex_for_one_location takes as input a
    network location and its index and a network.  It output the new
    network where the parameter network_location and other
    network_locations are modified following the reindexing of the
    network_location.  Modifications are twofold. In the parameter
    network_location, sharp symbols are suppressed.  The indexes of
    the suppressed SHARP symbols are kept. In other network locations,
    for each SHARP symbol suppressed, each formula that refers to a
    formula that is at an index after the suppressed location needs to
    have its second coordinate decreased.  **)
let reindex (n:network):network =
  let reindex_for_one_location (index:int) (l:network_location) (n:network):network =
    let sindexes = get_sharp_indexes l in
    let sindexes = List.sort (fun a b -> b -  a) sindexes in
    Network.mapi
      (
	fun k l ->
	  if k <> index then
	    (
	      List.fold_left (fun acc elt -> decrement_pointers_after_index acc index elt) l sindexes
	    )
	  else
	    l
      )
      n in
  let new_net = ref n in
  for cpt = 0 to (Network.cardinal n - 1) do
    new_net := reindex_for_one_location cpt (Network.find cpt !new_net) !new_net
  done;
  Network.map (fun elt -> remove_sharps elt) !new_net
      
    
(** Retrieve a formula in a network given its coordinates.  **)
let get_formula (n:network) (compo_index:int) (form_index:int) : dltl =
  get (Network.find compo_index n) form_index

(** Computation of formulae that should automatically respawn.
    The function takes as input an architecture, a network, and the
    formula that has been used to generate this network.  It scans the
    network and each time it encouters an until operator, it marks the
    cells referred by the pointers appearing in children as automatically
    respawning.  **)
let compute_respawn (archi:architecture) (n:network) (phi:dltl) : coordinates list =
  let rec cr (f:dltl) (b:bool) : coordinates list =
    match f,b with
      | Neg fp, b -> cr fp b
      | Next fp, b -> cr fp true
      | Or (fp, fpp), b
      | And (fp, fpp), b
      | Xor (fp, fpp), b
      | Iff (fp, fpp), b
      | Imp (fp, fpp), b -> (cr fp b) @ (cr fpp b)	
      | Until (fp, fpp), b 
      | Wuntil (fp, fpp), b -> (cr fp true) @ (cr fpp true)
      | Glob fp, b 
      | Ev fp, b -> cr fp true
      | Dists (i,j, _), false -> cr (get_formula n i j) false
      | Dists (i,j, _), true -> (i,j)::(cr (get_formula n i j) true)
      | _, _ -> []
  in cr (main_formula archi n phi) false


(** ///////////////////////////////////////////////////////////////
    Coordinates
    /////////////////////////////////////////////////////// **)
let main_coord (archi: architecture) (n:network) (f:dltl): coordinates =
  let c = chc f archi 0
  in (c, length (Network.find c n) - 1)

  (** ///////////////////////////////////////////////////////////
    Referents
    /////////////////////////////////////////////////////////// **)

type referent = coordinates
type referents = referent list

let stringrep_referents (r:referents) : string =
  stringrep_intint_list r
    
(** Rereferent of a Dtll formula. It scans the formula and aggregates
    in a list the coordinates in pointers **)
let referents_of_formula (phi:dltl) : referents =
  let rec refs_f (phi:dltl) : referents =
    match phi with
      | True | False | SHARP | Var _ -> []
      | Or (phip, phipp)
      | Xor (phip, phipp)
      | And (phip, phipp) 
      | Iff (phip, phipp)
      | Imp (phip, phipp)
      | Until (phip, phipp)
      | Wuntil (phip, phipp) -> (refs_f phip)@(refs_f phipp)
      | Next phip
      | Neg phip
      | Glob phip
      | Ev phip -> refs_f phip
      | Dists (x,y,_) 
      | Distd(x,y,_) -> [(x,y)] in
  suppress_duplicates (refs_f phi)

(** Referents of a network location. It scans the network location,
    calling function referent_of_formula on each cell -- NOT USED **)
let referents_of_network_location_aslist (l:network_location) : referents =
  fold_left (fun acc elt -> acc@(referents_of_formula elt)) [] l

(** Type referents_network_location: referents **)
type referents_network_location = referents

(** Computation of referents of a network location. It scans the
    network_location (array of Dltl formulae) and acccumulates the
    referents of each cell by applying function referents_of_formula
    on each cell using fold_left **)
let referents_of_network_location (l:network_location) : referents_network_location =
  fold_left (fun acc elt -> acc@(referents_of_formula elt)) [] l

let stringrep_referents_network_location (refs:referents_network_location) : string =
  stringrep_referents refs
  
(** Type referents_network_location: an array of referents **)
type referents_network = referents_network_location array


(** Computation of referents of a network. It creates an array of
    size of the network. The network is scanned entry-by-entry, and,
    for each network_location, it adds the referents of the network
    location at the index given by the key (integer for the current
    implementation of network) **)
let referents_of_network (net:network) : referents_network =
  let size = Network.cardinal net in
  let result = Array.make (size) [] in
  Network.fold (fun k elt acc -> result.(k) <- (referents_of_network_location elt)) net ();
  result

let stringrep_referents_network (refs:referents_network) : string =
  stringrep_array refs stringrep_referents_network_location true

(** ///////////////////////////////////////////////////////////
    Referrers
    //////////////////////////////////////////////////////**)

(** Type referrer. A referrer is simply a coordinate of a
    network_location **)
type referrer = coordinate
(** Type referrers. Referrers (for a network cell) is a list of
    referrer(s) **)
type referrers = referrer list

let stringrep_referrers (refs:referrers) : string = stringrep_int_list refs

(** The referrers of a cell is the list of components that refer
    to this cell. The cell is given by its coordinates **)
let referrers_of_cell (i:coordinate) (j:coordinate) (net:network) : referrers =
  let is_network_loc_referring (l:network_location) : bool =
    List.mem (i,j) (referents_of_network_location_aslist l) in
  Network.fold (fun k elt acc -> (if (is_network_loc_referring elt) then [k] else [])@acc) net []

type referrers_network_location = referrers array

let stringrep_referrers_network_location (refs:referrers_network_location) : string =
  stringrep_array refs stringrep_referrers false
    
let referrers_of_network_location (index:coordinate) (l:network_location) (net:network) : referrers_network_location =
  to_array (mapi (fun i elt -> referrers_of_cell index i net) l)

type referrers_network = referrers_network_location array

let stringrep_referrers_network (refs:referrers_network) : string =
  stringrep_array refs stringrep_referrers_network_location true
    
let referrers_of_network (net:network) : referrers_network =
  let size = Network.cardinal net in
  let result = Array.make (size) (Array.make 0 []) in
  Network.fold (fun k elt acc -> result.(k) <- (referrers_of_network_location k elt net)) net ();
  result

    
