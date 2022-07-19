open Ltl

(**
 Representing a LTL_d formula.
 Notice that Var is of type string now. 
**)

type dltl = 
    True
  | False
  | Var of string
  | Or of dltl * dltl
  | And of dltl * dltl
  | Neg of dltl
  | Iff of dltl * dltl
  | Imp of dltl * dltl
  | Until of dltl * dltl
  | Wuntil of dltl * dltl 
  | Next of dltl
  | Glob of dltl
  | Ev of dltl
  | Xor of dltl * dltl
  | SHARP                 
  | Dists of int * int * dltl
  (* This should appear only in static networks (network.ml) *)
  | Distd of int * int * int
(* This should appear only in dynamic networks (dyn_net.ml) *)

let rec dltl_2_ltl (phi:dltl) : ltl =
  match phi with
    | True -> True
    | False -> False
    | Var x -> Var x
    | Or (phi1, phi2) -> Or (dltl_2_ltl phi1, dltl_2_ltl phi2)
    | And (phi1, phi2) -> And (dltl_2_ltl phi1, dltl_2_ltl phi2) 
    | Neg (phi1) -> Neg (dltl_2_ltl phi1)
    | Iff (phi1, phi2) -> Iff (dltl_2_ltl phi1, dltl_2_ltl phi2)
    | Imp (phi1, phi2) -> Imp (dltl_2_ltl phi1, dltl_2_ltl phi2)
    | Until (phi1, phi2) -> Until (dltl_2_ltl phi1, dltl_2_ltl phi2)
    | Wuntil (phi1, phi2) -> Wuntil (dltl_2_ltl phi1, dltl_2_ltl phi2) 
    | Next (phi1) -> Next (dltl_2_ltl phi1)
    | Glob (phi1) -> Glob (dltl_2_ltl phi1)
    | Ev (phi1) -> Ev (dltl_2_ltl phi1)
    | Xor (phi1, phi2) -> Xor (dltl_2_ltl phi1, dltl_2_ltl phi2)
    | SHARP                 
    | _ -> failwith "This formula cannot be converted to LTL"

let rec ltl_2_dltl (phi:ltl) : dltl =
  match phi with
    | True -> True
    | False -> False
    | Var x -> Var x
    | Or (phi1, phi2) -> Or (ltl_2_dltl phi1, ltl_2_dltl phi2)
    | And (phi1, phi2) -> And (ltl_2_dltl phi1, ltl_2_dltl phi2) 
    | Neg (phi1) -> Neg (ltl_2_dltl phi1)
    | Iff (phi1, phi2) -> Iff (ltl_2_dltl phi1, ltl_2_dltl phi2)
    | Imp (phi1, phi2) -> Imp (ltl_2_dltl phi1, ltl_2_dltl phi2)
    | Until (phi1, phi2) -> Until (ltl_2_dltl phi1, ltl_2_dltl phi2)
    | Wuntil (phi1, phi2) -> Wuntil (ltl_2_dltl phi1, ltl_2_dltl phi2) 
    | Next (phi1) -> Next (ltl_2_dltl phi1)
    | Glob (phi1) -> Glob (ltl_2_dltl phi1)
    | Ev (phi1) -> Ev (ltl_2_dltl phi1)
    | Xor (phi1, phi2) -> Xor (ltl_2_dltl phi1, ltl_2_dltl phi2)
    | SHARP                 
    | _ -> failwith "This formula cannot be converted to DLTL"

let rec size (f:dltl) =
  match f with
      True -> 1
    | False -> 1
    | Var x -> 1
    | Or (f1,f2) | And (f1,f2) | Iff (f1,f2) | Imp (f1,f2) | Until (f1,f2) | Wuntil (f1,f2) | Xor (f1,f2)
      -> 1+ size f1 + size f2
    | Neg f | Next f | Glob f | Ev f -> 1 + size f
    | SHARP -> 0
    | Dists (_,_,_) | Distd (_,_,_) -> 1


let string_rep_operator =
let rec srep f =
  match f with
      Var x ->  "Var \""^x^"\""
    | True ->  "True"
    | False ->  "False"
    | SHARP -> "#"
    | Glob x -> "Glob ("^srep x^")"
    | Ev x ->  "Ev ("^ srep x^")"
    | Neg x ->  "Neg ("^ srep x^")"
    | Next x ->  "Next ("^ srep x^  ")"
    | And (x, y) ->  "And ("^ srep x^  ", "^ srep y^  ")"
    | Or (x, y) ->  "Or ("^ srep x^  ", "^srep y^  ")"
    | Until (x, y) ->  "Until ("^ srep x^  ", "^srep y^  ")"
    | Wuntil (x, y) ->  "W-Until ("^ srep x^  ", "^srep y^  ")"
    | Iff (x, y) ->  "Iff ("^ srep x^  ", "^srep y^  ")"
    | Imp (x, y) ->  "Imp ("^ srep x^  ", "^srep y^  ")"
    | Xor (x,y) -> "Xor ("^srep x^","^srep y^")"
(**    | Dists (i,j, fp) -> "Dist (" ^ string_of_int i ^", "^string_of_int j^", "^srep fp^")" **)
  | Dists (i,j, fp) -> "Dists (" ^ string_of_int i ^", "^string_of_int j^")" 
    | Distd (i,j,t) -> "Distd (" ^ string_of_int i ^", "^string_of_int j^", "^string_of_int t^")" 
in srep

let rec string_rep_aswritten = function
  | Var x ->  x
  | True ->  "tt"
  | False ->  "ff"
  | SHARP -> "#"
  | Glob x -> "G ("^string_rep_aswritten x^")"
  | Ev x ->  "F ("^ string_rep_aswritten x^")"
  | Neg x ->  "! ("^ string_rep_aswritten x^")"
  | Next x ->  "o ("^ string_rep_aswritten x^  ")"
  | And (x, y) -> string_rep_aswritten x^ " /\\ "^string_rep_aswritten y
  | Or (x, y) ->  string_rep_aswritten x^  " \\/ "^string_rep_aswritten y
  | Until (x, y) ->  "("^string_rep_aswritten x^") U ("^string_rep_aswritten y^")"
  | Wuntil (x, y) ->  string_rep_aswritten x^") W ("^string_rep_aswritten y
  | Iff (x, y) ->  string_rep_aswritten x^  "<=>"^string_rep_aswritten y
  | Imp (x, y) ->  string_rep_aswritten x^"=>"^string_rep_aswritten y
  | Xor (x,y) -> string_rep_aswritten x^" + "^string_rep_aswritten y
  | Dists (i,j,fp) -> "<|" ^ string_of_int i ^", "^string_of_int j^", "^(string_rep_aswritten fp)^"|>"
  | Distd (i,j,t) -> "<|" ^ string_of_int i ^", "^string_of_int j^", "^string_of_int t^"|>"

let string_rep = string_rep_operator

let string_rep_list (l: dltl list): string =
  let s = List.fold_left (fun acc elt -> (if (acc = "") then "" else acc^",")^(string_rep elt)) "" l in
  "["^s^"]"
      
let show_formula f = print_endline (string_rep f)	
	
let rec number_of_next (form:dltl) =
	match form with
		Next f -> 1+number_of_next f
		| _ -> 0
	
(* Retrieve a formula nested with previous operator, i.e. retrieves Phi in X^m Phi 
It assumes the input formula to be in the form X^m Phi*)	
let rec retrieve_nested_formula_in_a_next (form:dltl) =
	match form with
		Next f -> retrieve_nested_formula_in_a_next f
		| f -> f





(*s This function takes a formula [f] and simplifies it according to
  the laws of Boolean algebra and LTL. *)
(* Here is an implementation of the simplification function implemented in Eagle *)
let rec simplify_eagle f =
  match f with
    | Xor (a,b) -> (match (simplify_eagle a), (simplify_eagle b) with
	| True, True -> False
	| True,f -> Neg f
	| e,f when (e=f) -> False 
	| f, True -> Neg f
	| False, f -> f
	| f, False -> f
	| e,f -> Xor (e,f)
	  
    )
    | And(a,b) -> (match (simplify_eagle a), (simplify_eagle b) with
	| True, True -> True
	| True, f -> simplify_eagle f
	| f, True -> simplify_eagle f
	| False, False -> False
	| False, f -> False
	| f, False -> False
	| f,g when (f=g) -> simplify_eagle a
	| f, Xor(g,h) ->simplify_eagle (Xor(And(f,g),And(f,h)))
	| SHARP, f -> f
    	| f , SHARP -> f
    	  
    	| e, Neg f when e = f -> False
    	| Neg e, f when e = f -> False
    	| e,f when ((e = Neg f) || (Neg e = f)) -> False
    	| e, f when (e = f) -> e
	  
    	| e, Glob f when (e = f) -> Glob f
    	| Glob f, e when (e = f) -> Glob f
    	  
    	| Neg e, Glob (And(f,g)) when ((e = f) || (e=g)) -> False
    	| Glob (And(f,g)),Neg e when ((e = f) || (e=g)) -> False
    	  
        | e, Ev f when (e = f) -> e
    	| Ev e, f when (e = f) -> e
    	  
    	| Neg e, Glob f when (e = f) -> False
    	| Glob f, Neg e when (e = f) -> False
    	  
    	| e, Glob f when ((Neg e = f) || (e = Neg f)) -> False
    	| Glob f, e when ((Neg e = f) || (e = Neg f)) -> False
    	  
    	  
    	| And(e,f), And(g,h) when ((e=g && f=h) || ((e=h) && f=g)) -> simplify_eagle (And(e ,g))
	  
	| And(e,f), Or(g,h) when ( e =  g) -> simplify_eagle ( And (e, f) )    						
	| And(e,f), Or(g,h) when ( e =  h) ->  simplify_eagle ( And (e, f) )
	| Or(g,h), And(e,f) when ( e =  g) ->  simplify_eagle ( And (e, f) )   						
	| Or(g,h), And(e,f) when ( e =  h) ->  simplify_eagle ( And (e, f) )    						
	  
    	| Neg e , And (f,g) when (( e =  f) || ( e= g)) -> False
    	| And (f,g), Neg e  when (( e =  f) || ( e= g)) -> False
    	| e , And (Neg f,g) when ( e = f) -> False
    	| e , And (f,Neg g) when ( e = g) -> False
    	| And (Neg f,g),e when ( e = f) -> False
    	| And (f,Neg g),e when ( e = g) -> False    						
    	  
    	| Neg e , Or (f,g) when ( e =  f)  ->   simplify_eagle ( And (Neg e, g) )
    	| Neg e , Or (f,g) when ( e =  g)  ->  simplify_eagle ( And (Neg e, f) )
    	| Or (f,g), Neg e when (e = f)  ->  simplify_eagle ( And (Neg e, g))
    	| Or (f,g), Neg e when (e = g)  ->  simplify_eagle ( And (Neg e, f))
    	  
    	| e, Or(f,g) when (( e =  f) || ( e= g)) ->  e
    	| Or(f,g),e  when (( e =  f) || ( e= g)) ->  e
   	| e, And(f,g) when ( e =  f) ->   (And(e, g))
   	| e, And(f,g) when ( e= g) ->  And (f, g)
	| And(f,g),e  when ( e =  f) -> And(f, g)
   	| And(f,g),e when ( e= g) ->  And (f, g)
   	  
   	| e,f -> And (e,f)

	  
    )
    | Or (a,b)-> simplify_eagle ( Xor( And(a,b), Xor(a,b) ) )
    | Neg a -> simplify_eagle (Xor (True,a))
    | Iff(a,b) -> simplify_eagle (Xor (Xor (True,a),b))
    | Imp (a,b) -> simplify_eagle (Xor(True,Xor(a,And(a,b))))
    | Wuntil (e,f) -> simplify_eagle (Or(Glob e, Until(e,f)))
    | Until (a, b) -> (match (simplify_eagle a), (simplify_eagle b) with 
    	True , _ -> True
    	| _, True -> True
    	| False, _ -> False
    	| e , False -> if (e=True) then True else False
    	| e, f when (e = f) -> e
    	| Neg e, f when (e = f) -> (Neg e)
    	| e, Neg f when (e = f) -> e
	  (**
    	| And(e,f),g when (e = g) -> simplify_eagle (Until (f,g))
    	| And(e,f),g when (f = g) -> simplify_eagle (Until (e,g))

    	| e, And(f,g) when (f = e) -> simplify_eagle (Until (e,g))
    	| e, And(f,g) when (g = e) -> simplify_eagle (Until (e,f))
	  **)
	| And(e,f),g -> simplify_eagle (And((Until(e,g)),(Until(f,g))))
	| e,Or(f,g) -> simplify_eagle (Or((Until(e,f)),(Until(e,g))))
	| e,Until (f,g) when (e=f) -> Until (f,g)
   	| e,f -> Until (e,f)
    )
    | Ev True -> True
    | Ev False -> False
    | Ev Ev e -> simplify_eagle (Ev e)
    | Ev e -> (match (simplify_eagle e) with 
	True -> True
	| False -> False
	| And (f,Ev(And(g,h))) when ((f =g) || (f=h)) -> Ev (And (g,h))
	| And (Ev(And(g,h)),f) when ((f =g) || (f=h)) -> Ev (And (g,h))
	| Or(f,g) -> simplify_eagle (Or(Ev f,Ev (g)))
	| Ev f -> Ev f
	| f -> Ev f
    )
    | Glob e -> (match (simplify_eagle e) with 
	True -> True
	| False -> False
	| Glob f -> Glob f
	| Or (f,Glob (Or(g,h))) when ((f=g) || (f=h)) -> simplify_eagle (Glob (Or(g,h)))
	| And (f,Glob (And(g,h))) when ((f=g) || (f=h)) -> simplify_eagle (Glob f) 
	| And (f, g) -> simplify_eagle (And (Glob f, Glob g))
	| f -> Glob f
    )
    | Next e -> (match (simplify_eagle e) with 
	And (f,g) -> And(Next f,Next g)
	| Or (f,g) -> Or (Next f, Next g)
	| f -> Next f
    )
    | f -> f
	      

let simplify_rec form =
  let rec simplify form :dltl =
    match form with
      | Neg e -> (match (simplify e) with 
    	  True -> False
    	  | False -> True
    	  | Neg f -> simplify f
    	  | Next f -> Next (simplify (Neg f))
    	(*
    	  | And (f,g) -> simplify (Or (Neg f,Neg g))
    	  | Or (f,g) -> simplify (And (Neg f, Neg g))
    	  | Glob f -> simplify(Ev (simplify (Neg f)))
    	  | Ev f -> simplify (Glob (Neg f))
    	*)
    	  | f -> Neg f
      )
      | And (a, b) -> (match (simplify a), (simplify b) with
    	  SHARP, f -> f
    	  | f , SHARP -> f
    	    
    	  | True , e -> e
    	  | e, True -> e
    	  | False, _ -> False
    	  | _, False -> False
    	  | e, Neg f when e = f -> False
    	  | Neg e, f when e = f -> False
    	  | e,f when ((e = Neg f) || (Neg e = f)) -> False
    	  | e, f when (e = f) -> e
	    
    	  | e, Glob f when (e = f) -> Glob f
    	  | Glob f, e when (e = f) -> Glob f
    	    
    	  | Neg e, Glob (And(f,g)) when ((e = f) || (e=g)) -> False
    	  | Glob (And(f,g)),Neg e when ((e = f) || (e=g)) -> False
    	    
          | e, Ev f when (e = f) -> e
    	  | Ev e, f when (e = f) -> e
    	    
    	  | Neg e, Glob f when (e = f) -> False
    	  | Glob f, Neg e when (e = f) -> False
    	    
    	  | e, Glob f when ((Neg e = f) || (e = Neg f)) -> False
    	  | Glob f, e when ((Neg e = f) || (e = Neg f)) -> False
    	    
    	    
    	  | And(e,f), And(g,h) when ((e=g && f=h) || ((e=h) && f=g)) -> simplify (And(e ,g))
	    
	  | And(e,f), Or(g,h) when ( e =  g) -> simplify ( And (e, f) )    						
	  | And(e,f), Or(g,h) when ( e =  h) ->  simplify ( And (e, f) )
	  | Or(g,h), And(e,f) when ( e =  g) ->  simplify ( And (e, f) )   						
	  | Or(g,h), And(e,f) when ( e =  h) ->  simplify ( And (e, f) )    						
	    
    	  | Neg e , And (f,g) when (( e =  f) || ( e= g)) -> False
    	  | And (f,g), Neg e  when (( e =  f) || ( e= g)) -> False
    	  | e , And (Neg f,g) when ( e = f) -> False
    	  | e , And (f,Neg g) when ( e = g) -> False
    	  | And (Neg f,g),e when ( e = f) -> False
    	  | And (f,Neg g),e when ( e = g) -> False    						
    	    
    	  | Neg e , Or (f,g) when ( e =  f)  ->   simplify ( And (Neg e, g) )
    	  | Neg e , Or (f,g) when ( e =  g)  ->  simplify ( And (Neg e, f) )
    	  | Or (f,g), Neg e when (e = f)  ->  simplify ( And (Neg e, g))
    	  | Or (f,g), Neg e when (e = g)  ->  simplify ( And (Neg e, f))
    	    
    	  | e, Or(f,g) when (( e =  f) || ( e= g)) ->  e
    	  | Or(f,g),e  when (( e =  f) || ( e= g)) ->  e
   	  | e, And(f,g) when ( e =  f) ->   (And(e, g))
   	  | e, And(f,g) when ( e= g) ->  And (f, g)
	  | And(f,g),e  when ( e =  f) -> And(f, g)
   	  | And(f,g),e when ( e= g) ->  And (f, g)
   	    
   	  | e,f -> And (e,f)
      )
	
      | Or (a, b) ->  (match (simplify a), (simplify b) with 
    	  True , _ -> True
    	  | _, True -> True
    	  | False, e -> e
    	  | e, False -> e
    	  | e, Neg f when e = f -> True
    	  | Neg e, f when e = f -> True
    	    
    	  | e, Glob f when e = f -> e
    	  | Glob f, e when e = f -> e
    	    
    	  | Neg e, Ev (Or (f,g)) when (e = f || e= g) -> True
    	  | Ev (Or (f,g)), Neg e when (e = f || e= g) -> True

    	    
    	  | Neg e, Ev f when (e = f) -> True
    	  | Ev f, Neg e when (e = f) -> True
    	    
    	  | e, Ev f when (e = f) -> Ev f
    	  | Ev f, e when (e = f) -> Ev f
    	    
    	  | e, Ev f when ((Neg e = f) || (e = (Neg f))) -> True
    	  | Ev f, e when (((Neg e) = f) || (e = (Neg f))) -> True
    	    
   	    
    	    
    	  | Or(e,f), Or(g,h) when ((e=g && f=h) || ((e=h) && (f=g))) -> simplify (Or(e , f))
    	  | And(e,f), Or(g,h) when (e = g) ->  simplify ( Or (e, And (Neg e, h)))
    	  | And(e,f), Or(g,h) when (e = h) ->  simplify ( Or (e, And (Neg e, g)))
    	  | Or(g,h), And(e,f) when (e = g) ->  simplify ( Or (e, And (Neg e, h)) )
    	  | Or(g,h), And(e,f) when (e = h) ->  simplify ( Or (e, And (Neg e, g)))
    	    
    	    
    	  | Neg e, Or (f,g) when (e=g || e=f) -> True
    	  | Or (f,g), Neg e when (e=g || e=f) -> True
    	    
    	  | e, Or(f,g) when (e = f) ->  Or(e, g)
    	  | e, Or(f,g) when (e = g) ->  Or( e, f)
    	  | Or(f,g),e when (e = f) ->  Or(e, g)
    	  | Or(f,g),e when ( e = g) ->  Or( e, f)
    	    
    	  | e, And(f,g) when ((e =  f) || (e = g)) ->   e
	  | And(f,g),e when ((e =  f) || (e = g)) ->   e
   	    
    	    
 	    
   	  | e, f when (e = f) -> e
   	  | e, f when (e = Neg f) -> True
   	  | e, f when (Neg e = f) -> True
   	  | e,f -> Or (e,f)
      )

      | Until (a, b) -> (match (simplify a), (simplify b) with 
    	  True , _ -> True
    	  | _, True -> True
    	  | False, _ -> False
    	  | e , False -> if (e=True) then True else False
    	  | e, f when (e = f) -> e
    	  | Neg e, f when (e = f) -> (Neg e)
    	  | e, Neg f when (e = f) -> e
	(**
    	   | And(e,f),g when (e = g) -> simplify (Until (f,g))
    	   | And(e,f),g when (f = g) -> simplify (Until (e,g))
    	   | e, And(f,g) when (f = e) -> simplify (Until (e,g))
    	   | e, And(f,g) when (g = e) -> simplify (Until (e,f))
	**)
	  | And(e,f),g -> simplify (And((Until(e,g)),(Until(f,g))))
	  | e,Or(f,g) -> simplify (Or((Until(e,f)),(Until(e,g))))
	  | e,Until (f,g) when (e=f) -> Until (f,g)
   	  | e,f -> Until (e,f)
      )		
	
      | Iff (a, b) -> (match (simplify a), (simplify b) with
          True, e -> e
          | e,True -> e
          | False, e -> simplify (Neg e)
          | e,False -> simplify (Neg e)
          | e, Neg f when (e = f)  -> False	
  	  | e,f when (e= f) -> True
    	  | e,f -> simplify (And (Imp (e,f), Imp (f,e)))
      )
      | Imp (a, b) -> (match (simplify a), (simplify b) with
    	  False,_ -> True
    	  | _,True -> True
    	  | True,e -> e
    	  | e,False -> simplify (Neg e)
    	  | e,f when (e=f) -> True
    	  | e,f -> simplify (Or (Neg e, f))
      )

      | Ev True -> True
      | Ev False -> False
      | Ev Ev e -> simplify (Ev e)
      | Ev e -> (match (simplify e) with 
    	  True -> True
    	  | False -> False
    	  | And (f,Ev(And(g,h))) when ((f =g) || (f=h)) -> Ev (And (g,h))
    	  | And (Ev(And(g,h)),f) when ((f =g) || (f=h)) -> Ev (And (g,h))
    	  | Or(f,g) -> simplify (Or(Ev f,Ev (g)))
    	  | Ev f -> Ev f
    	  | f -> Ev f
      )
      | Glob e -> (match (simplify e) with 
    	  True -> True
    	  | False -> False
    	  | Glob f -> Glob f
    	  | Or (f,Glob (Or(g,h))) when ((f=g) || (f=h)) -> simplify (Glob (Or(g,h)))
    	  | And (f,Glob (And(g,h))) when ((f=g) || (f=h)) -> simplify (Glob f) 
    	  | And (f, g) -> simplify (And (Glob f, Glob g))
    	  | f -> Glob f
      )
      | Next e -> Next (simplify e) 
	
      | Wuntil (e,f) -> simplify (Or(Glob e, Until(e,f)))
      | SHARP -> SHARP
      | _ -> form
  in
  let f = simplify form in
  if f = form then f else simplify f


let simp = simplify_rec

let simplify_by_dereferring (phi:dltl) : dltl =
  let rec build_forms_pointers_association (phi:dltl) : (dltl*(int*int)) list =
    match phi with
      | Dists (i, j, f) -> [(f,(i,j))]
      | Or (fp, fpp) |  And (fp, fpp) | Iff (fp, fpp) | Imp (fp, fpp) | Until (fp, fpp) | Wuntil (fp, fpp) | Xor (fp, fpp)
	-> (build_forms_pointers_association fp)@(build_forms_pointers_association fpp) 
      | Neg fp | Next fp | Glob fp | Ev fp -> build_forms_pointers_association fp
      | True | False | Var _ | SHARP | Distd (_,_,_) -> []

  in
  let associations = build_forms_pointers_association phi in
  let rec substitute_pointers_by_formula (phi:dltl) : dltl =
    let rec sub k phi = match phi with
      | Dists (i, j, f) -> if k=0 then f else sub (k-1) f
      | Or (fp, fpp) -> Or (sub k fp, sub k fpp)
      | And (fp, fpp) -> And  (sub k fp, sub k fpp)
      | Iff (fp, fpp) -> Iff (sub k fp, sub k fpp)
      | Imp (fp, fpp) -> Imp (sub k fp, sub k fpp)
      | Until (fp, fpp) -> Until  (sub k fp, sub k fpp)
      | Wuntil (fp, fpp) -> Wuntil (sub k fp, sub k fpp)
      | Xor (fp, fpp) -> Xor (sub k fp, sub k fpp)
      | Neg fp -> Neg (sub k fp) | Next fp -> Next (sub k fp) | Glob fp -> Glob (sub k fp) | Ev fp -> Ev (sub k fp)
      | f -> f
    in sub 1 phi
  and reconstruct_formula (phi:dltl) : dltl =
    try
      let (f, (x,y)) = List.find (fun (f, (x,y)) -> f = phi) associations in
      Dists (x, y, f)
    with Not_found ->
      match phi with
	| Or (fp, fpp) -> Or (reconstruct_formula fp, reconstruct_formula fpp)
	| And (fp, fpp) -> And (reconstruct_formula fp, reconstruct_formula fpp) | Iff (fp, fpp) -> Iff (reconstruct_formula fp, reconstruct_formula fpp)  | Imp (fp, fpp) -> Imp (reconstruct_formula fp, reconstruct_formula fpp) | Until (fp, fpp) -> Until (reconstruct_formula fp, reconstruct_formula fpp) | Wuntil (fp, fpp) -> Wuntil (reconstruct_formula fp, reconstruct_formula fpp) | Xor (fp, fpp) -> Xor (reconstruct_formula fp, reconstruct_formula fpp)
	| Neg fp -> Neg (reconstruct_formula fp)
	| Next fp -> Next (reconstruct_formula fp) | Glob fp -> Glob (reconstruct_formula fp) | Ev fp -> Ev (reconstruct_formula fp)
	| f -> f
  in
  let phi_subed = substitute_pointers_by_formula phi in
  let phi_subed_simped = simp phi_subed in
  if (phi_subed = phi_subed_simped) then
    (
(**      print_endline ("\n\n\nphi_subed:"^(string_rep phi_subed)^",\nsubed simped: "^(string_rep phi_subed_simped));
**)     phi
    )
  else
    (
      let phip = reconstruct_formula phi_subed_simped in
      (* print_endline ("\n\nBINGO:\nphi:"^(string_rep phi)^",\nsimp: "^(string_rep phip)); *)
      phip
    )
  
(*s Returns the closure of LTL formula [f] in the form of a list, e.g.,
   [closure (Glob (Var "a"))] would return [[Var "a"; Glob (Var
   "a")]]. *)

let rec closure f =
  match f with
      And (a, b) -> f::(closure a)@(closure b)
    | Or (a, b) -> f::(closure a)@(closure b)
    | Var a -> [Var a]
    | Neg a -> f::(closure a)
    | Imp (a, b) -> closure (simp (Imp (a, b)))
    | Iff (a, b) -> closure (simp (Iff (a, b)))
    | Next a -> f::(closure a)
    | Until (a, b) -> f::(closure a)@(closure b)
    | Wuntil (a, b) -> f::(closure a)@(closure b)
    | Glob a -> f::(closure a)
    | Ev a -> f::(closure a)
    | Xor (a, b) -> f::(closure a)@(closure b)
    | _ -> [f; Neg f]
    
let rec urgent_closure f =
	match  f with 
	 And (a, b) | Or (a,b) |  Iff (a,b) | Imp(a,b) | Xor (a,b) -> (urgent_closure a)@(urgent_closure b) 
    | Neg a -> urgent_closure a
    | _ -> []
    

(*s Returns a list of used variables in a formula [f], e.g., if
   [f] is [Glob (Var "a")], then [[Var "a"]] is returned. *)

let rec variables f = 
  match f with
     And (a, b) -> (variables a) @ (variables b)
    | Until (a, b) -> (variables a) @ (variables b)
    | Wuntil (a, b) -> (variables a) @ (variables b)
    | Or (a, b) -> (variables a) @ (variables b)
    | Glob a -> variables a
    | Neg a -> variables a
    | Ev a -> variables a
    | Next a -> variables a
    | Var x -> [Var x]
    | Xor (a, b) -> (variables a) @ (variables b)
    | _ -> []

(** Apply substitution of pointer by a pointer **)
let substitute_pointer_by_pointer (f:dltl) (i:int) (j:int) (ip:int) (jp:int):dltl =
  let rec sub (f:dltl):dltl =
    match f with
      | Or (fp, fpp) -> Or (sub fp, sub fpp)
      | And (fp, fpp) -> And (sub fp, sub fpp)
      | Glob fp -> Glob (sub fp)
      | Neg fp -> Neg (sub fp)
      | Ev fp -> Ev (sub fp)
      | Next fp -> Next (sub fp)
      | Dists (x, y, p) -> if (x=i && y =j) then Dists (ip, jp, p) else f
      | Until (fp, fpp) -> Until (sub fp, sub fpp)
      | f -> f
  in sub f

let substitute_pointer_by_pointer_list (f:dltl) (idxl: (int*int) list) (ip:int) (jp:int):dltl =
  List.fold_left (fun acc (i,j) -> substitute_pointer_by_pointer acc i j ip jp) f idxl

