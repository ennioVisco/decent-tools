(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com> 
  2011 Yli�s Falcone <ylies.falcone@gmail.com>
  2013 Christian Colombo
   Code is released under the GPL license version 3. *)

type ltl =
    True
  | False
  | Var of string
  | Or of ltl * ltl
  | And of ltl * ltl
  | Neg of ltl
  | Iff of ltl * ltl
  | Imp of ltl * ltl
  | Until of ltl * ltl
  | Wuntil of ltl * ltl 
  | Next of ltl
  | Glob of ltl
  | Ev of ltl
  | Previous of ltl
  | Xor of ltl * ltl
  | SHARP
  
let rec size (f:ltl) =
  match f with
      True -> 1
    | False -> 1
    | Var x -> 1
    | Or (f1,f2) -> 1 + size f1 + size f2
    | And (f1,f2) -> 1 + size f1 +size f2
    | Neg f -> 1 + size f
    | Iff (f1,f2) -> 1 + size f1 + size f2
    | Imp (f1,f2) -> 1 + size f1 + size f2
    | Until (f1,f2) -> 1 + size f1 + size f2
    | Wuntil (f1,f2) -> 1 + size f1 + size f2
    | Next f -> 1 + size f
    | Glob f -> 1 + size f
    | Ev f -> 1 + size f
    | Previous f -> 1 + size f
    | Xor (f1,f2) -> 1 + size f1 + size f2
    | SHARP -> 0

let rec size_test (f: ltl) (n: int) = 
  match f with
      True -> n
    | False -> n
    | Var x -> n
    | Or (f1,f2) -> n + size_test f1 (n+1) + size_test f2 (n+1)
    | And (f1,f2) -> n + size_test f1 (n+1) + size_test f2 (n+1)
    | Neg f -> n + size_test f (n+1)
    | Iff (f1,f2) -> n + size_test f1 (n+1) + size_test f2 (n+1)
    | Imp (f1,f2) -> n + size_test f1 (n+1) + size_test f2 (n+1)
    | Until (f1,f2) -> n + size_test f1 (n+1) + size_test f2 (n+1)
    | Wuntil (f1,f2) -> n + size_test f1 (n+1) + size_test f2 (n+1)
    | Next f -> n + size_test f (n+1) 
    | Glob f -> n + size_test f (n+1) 
    | Ev f -> n + size_test f (n+1)
    | Previous f -> n + size_test f (n+1)
    | Xor (f1,f2) -> n + size_test f1 (n+1) + size_test f2 (n+1)
    | SHARP -> 0
(** Computes the nb of characters that are necessary to encode
    the formula in string representation.  Parenthesis are assumed to be
    systematically used to avoid ambiguity and a comma (one character) is
    used to separate formulae in binary operators.  Examples: Glob (Ev
    (Var a)), And (f1, f2) requires 4 symbols in addition to the nb of
    symbols to encode f1 and f2.  **)
      
let nb_characters_in_encoding (f:ltl) : int =
  let rec nb (f:ltl) : int =
      match f with
      True -> 1
    | False -> 1
    | Var x -> 2
    | Or (f1,f2) -> 4 + size f1 + size f2
    | And (f1,f2) -> 4 + size f1 +size f2
    | Neg f -> 4 + size f
    | Iff (f1,f2) -> 4 + size f1 + size f2
    | Imp (f1,f2) -> 4 + size f1 + size f2
    | Until (f1,f2) -> 4 + size f1 + size f2
    | Wuntil (f1,f2) -> 4 + size f1 + size f2
    | Next f -> 3 + size f
    | Glob f -> 3 + size f
    | Ev f -> 3 + size f
    | Previous f -> 3 + size f
    | Xor (f1,f2) -> 4 + size f1 + size f2
    | SHARP -> 1
  in nb f
      
(*s This function prints a formula [f] on the standard output. *)
      
(*i This note was relevant when ltl was of generic type:
  
  \textbf{Caveat}: However, right now it does not reflect the generic
  type defined in the data structure above.  What I ultimately want to
  do is restrict the data structure such that ['a] can only be of a
  type which provides its own [show] function, similar in style to
  type classes in Haskell using the [deriving] keyword.  i*)

	
let rec string_rep f =
  match f with
      Var x ->  "Var \""^x^"\""
    | True ->  "True"
    | False ->  "False"
    | SHARP -> "#"
    | Glob x -> "Glob ("^string_rep x^")"
    | Ev x ->  "Ev ("^ string_rep x^")"
    | Neg x ->  "Neg ("^ string_rep x^")"
    | Next x ->  "Next ("^ string_rep x^  ")"
    | And (x, y) ->  "And ("^ string_rep x^  ", "^ string_rep y^  ")"
    | Or (x, y) ->  "Or ("^ string_rep x^  ", "^string_rep y^  ")"
    | Until (x, y) ->  "Until ("^ string_rep x^  ", "^string_rep y^  ")"
    | Wuntil (x, y) ->  "W-Until ("^ string_rep x^  ", "^string_rep y^  ")"
    | Iff (x, y) ->  "Iff ("^ string_rep x^  ", "^string_rep y^  ")"
    | Imp (x, y) ->  "Imp ("^ string_rep x^  ", "^string_rep y^  ")"
    | Previous x -> "Previous ("^string_rep x^")"
    | Xor (x,y) -> "Xor ("^string_rep x^","^string_rep y^")"

let rec string_rep_simple f =
	match f with
			Var x ->  x
		| True ->  "True"
		| False ->  "False"
		| SHARP -> "#"
		| Glob x -> "G ("^string_rep_simple x^")"
		| Ev x ->  "F ("^ string_rep_simple x^")"
		| Neg x ->  "!("^ string_rep_simple x^")"
		| Next x ->  "X ("^ string_rep_simple x^  ")"
		| And (x, y) ->  "["^ string_rep_simple x^  " & "^ string_rep_simple y^  "]"
		| Or (x, y) ->  "["^ string_rep_simple x^  " | "^string_rep_simple y^  "]"
		| Until (x, y) ->  "["^ string_rep_simple x^  " U "^string_rep_simple y^  "]"
		| Wuntil (x, y) ->  "["^ string_rep_simple x^  " W "^string_rep_simple y^  "]"
		| Iff (x, y) ->  "["^ string_rep_simple x^  " <=> "^string_rep_simple y^  "]"
		| Imp (x, y) ->  "["^ string_rep_simple x^  ",=> "^string_rep_simple y^  "]"
		| Previous x -> "X! ("^string_rep_simple x^")"
		| Xor (x,y) -> "(["^string_rep_simple x^" XOR "^string_rep_simple y^")"

let rec string_rep_min f = 
	match f with
			Var x ->  x
		| True ->  "True"
		| False ->  "False"
		| SHARP -> "#"
		| Glob x -> "G("^string_rep_min x^")"
		| Ev x ->  "F("^ string_rep_min x^")"
		| Neg x ->  "!("^ string_rep_min x^")"
		| Next x ->  "X("^ string_rep_min x^  ")"
		| And (x, y) ->  "["^ string_rep_min x^  "&"^ string_rep_min y^  "]"
		| Or (x, y) ->  "["^ string_rep_min x^  "|"^string_rep_min y^  "]"
		| Until (x, y) ->  "["^ string_rep_min x^  "U"^string_rep_min y^  "]"
		| Wuntil (x, y) ->  "["^ string_rep_min x^  "W"^string_rep_min y^  "]"
		| Iff (x, y) ->  "["^ string_rep_min x^  "<=>"^string_rep_min y^  "]"
		| Imp (x, y) ->  "["^ string_rep_min x^  "=>"^string_rep_min y^  "]"
		| Previous x -> "X! ("^string_rep_min x^")"
		| Xor (x,y) -> "(["^string_rep_min x^"XOR"^string_rep_min y^")"

let string_rep_tree (f: ltl) =
	let rec indent (n: int) =
		match n with
				0 -> ""
			| _ -> "_ " ^ indent (n-1)

	in let rec string_rep_tree_rec (f: ltl) (n: int) =
		match f with
				Var x ->  (indent n) ^ x ^ "\n"
			| True -> (indent n) ^ "True\n"
			| False -> (indent n) ^  "False\n"
			| SHARP -> (indent n) ^ "#\n"
			| Glob x -> (indent n) ^ "G\n" ^string_rep_tree_rec x (n + 1)
			| Ev x -> (indent n) ^  "F\n" ^ string_rep_tree_rec x (n + 1)
			| Neg x -> (indent n) ^  "Not\n"^ string_rep_tree_rec x (n + 1)
			| Next x -> (indent n) ^  "X\n"^ string_rep_tree_rec x (n + 1)
			| And (x, y) -> (indent n) ^  "And\n"^ string_rep_tree_rec x (n + 1) ^ string_rep_tree_rec y (n + 1)  
			| Or (x, y) -> (indent n) ^  "Or\n"^ string_rep_tree_rec x (n + 1) ^ string_rep_tree_rec y (n + 1)  
			| Until (x, y) -> (indent n) ^  "Until\n"^ string_rep_tree_rec x (n + 1) ^ string_rep_tree_rec y (n + 1)  
			| Wuntil (x, y) -> (indent n) ^  "Wuntil\n"^ string_rep_tree_rec x (n + 1) ^ string_rep_tree_rec y (n + 1)  
			| Iff (x, y) -> (indent n) ^  "Iff\n"^ string_rep_tree_rec x (n + 1) ^ string_rep_tree_rec y (n + 1)  
			| Imp (x, y) -> (indent n) ^  "Imp\n"^ string_rep_tree_rec x (n + 1) ^ string_rep_tree_rec y (n + 1)  
			| Previous x -> (indent n) ^ "X!\n"^string_rep_tree_rec x (n + 1)
			| Xor (x,y) -> (indent n) ^ "Xor\n"^string_rep_tree_rec x (n + 1) ^ string_rep_tree_rec y (n + 1)

	in "\n" ^ string_rep_tree_rec f 0
	

let print_formula f =
	print_endline(string_rep f);
	print_endline(string_rep_simple f);
	print_endline ""


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
    | Previous  e -> (match (simplify_eagle e) with 
	And (f,g) -> simplify_eagle (And(Previous f,Previous g))
	| Or (f,g) -> simplify_eagle (Or (Previous f, Previous g))
	| f -> Previous f
    )
      
    | _ -> f



let rec simplify form =
  match form with
    | Neg e -> (
				match (simplify e) with 
						True -> False
					| False -> True 
					| Neg f -> simplify f
					| Next f -> Next (simplify (Neg f))
					
					| And (f,g) -> simplify (Or (Neg f,Neg g))
					| Or (f,g) -> simplify (And (Neg f, Neg g))
					| Glob f -> simplify(Ev (simplify (Neg f)))
					| Ev f -> simplify (Glob (Neg f))
					
					| f -> Neg f
    )
    | And (a, b) -> (
				match (simplify a), (simplify b) with
						SHARP, f -> f
					| f , SHARP -> f

					| True , e -> e
					| e, True -> e
					| False, _ -> False
					| _, False -> False
					| e, Neg f when e = f -> False
					| Neg e, f when e = f -> False
					| e, f when ((e = Neg f) || (Neg e = f)) -> False
					| e, f when (e = f) -> e
				
					| e, Glob f when (e = f) -> Glob f
					| Glob f, e when (e = f) -> Glob f
						
					| Neg e, Glob (And(f,g)) when ((e = f) || (e=g)) -> False
					| Glob (And(f,g)), Neg e when ((e = f) || (e=g)) -> False
						
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
					
					| Or(e,f), Or(g,h) when (e = g) -> simplify (Or (e, And (f, h)))
					| Or(e,f), Or(g,h) when (e = h) -> simplify (Or (e, And (f, g)))
					| Or(e,f), Or(g,h) when (f = g) -> simplify (Or (f, And (e, h)))
					| Or(e,f), Or(g,h) when (f = h) -> simplify (Or (f, And (e, g)))
				
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
					| Or(f,g), e when (( e =  f) || ( e= g)) ->  e
					| e, And(f,g) when ( e =  f) ->   (And(e, g))
					| e, And(f,g) when ( e= g) ->  And (f, g)
					| And(f,g),e  when ( e =  f) -> And(f, g)
					| And(f,g),e when ( e= g) ->  And (f, g)

					| Until (e, f), Until (g, h) when (f = h) -> Until (And (e, g), f)

					| e,f -> And (e,f)
    )
    | Or (a, b) ->  (
				match (simplify a), (simplify b) with 
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
					| And(e,f), Or(g,h) when (e = g) ->  simplify (Or (e, h))
					| And(e,f), Or(g,h) when (e = h) ->  simplify (Or (e, g))
					| And(e,f), Or(g,h) when (f = g) ->  simplify (Or (f, h))
					| And(e,f), Or(g,h) when (f = h) ->  simplify (Or (f, g))
					| Or(g,h), And(e,f) when (e = g) ->  simplify (Or (e, h))
					| Or(g,h), And(e,f) when (e = h) ->  simplify (Or (e, g))
					| Or(g,h), And(e,f) when (f = g) ->  simplify (Or (f, h))
					| Or(g,h), And(e,f) when (f = h) ->  simplify (Or (f, g))

					| e, And(Neg f, g) when (e = f) -> simplify (Or (e, g))
					| Neg e, And(f, g) when (e = f) -> simplify (Or (Neg e, g))
					| e, And(g, Neg f) when (e = f) -> simplify (Or (e, g))
					| Neg e, And(g, f) when (e = f) -> simplify (Or (Neg e, g))
					| And(Neg f, g), e when (e = f) -> simplify (Or (e, g))
					| And(f, g), Neg e when (e = f) -> simplify (Or (Neg e, g))
					| And(g, Neg f), e when (e = f) -> simplify (Or (e, g))
					| And(g, f), Neg e when (e = f) -> simplify (Or (Neg e, g))

					| And(e,f), And(g,h) when (e = g) -> simplify (And (e, Or (f, h)))
					| And(e,f), And(g,h) when (e = h) -> simplify (And (e, Or (g, h)))
					| And(e,f), And(g,h) when (f = g) -> simplify (And (f, Or (e, h)))
					| And(e,f), And(g,h) when (f = h) -> simplify (And (f, Or (e, g)))
						
					| Neg e, Or (f,g) when (e=g || e=f) -> True
					| Or (f,g), Neg e when (e=g || e=f) -> True
						
					| e, Or(f,g) when (e = f) ->  Or(e, g)
					| e, Or(f,g) when (e = g) ->  Or( e, f)
					| Or(f,g), e when (e = f) ->  Or(e, g)
					| Or(f,g), 	e when ( e = g) ->  Or( e, f)
						
					| e, And(f,g) when ((e =  f) || (e = g)) ->   e
					| And(f,g),e when ((e =  f) || (e = g)) ->   e
					
					| e, f when (e = f) -> e
					| e, f when (e = Neg f) -> True
					| e, f when (Neg e = f) -> True
					(* seed 9728 *)
					| Until (e, f), Until (g, h) when (e = g) -> Until (e, Or (f, h))
					| e,f -> Or (e,f)
    )
    | Until (a, b) -> (
				match (simplify a), (simplify b) with 
						True , _ -> True
					| _, True -> True
					| False, _ -> False
					| e , False -> if (e=True) then True else False
					| e, f when (e = f) -> e
					| Neg e, f when (e = f) -> (Neg e)
					| e, Neg f when (e = f) -> e
					(** 
					| And(e,f), g when (e = g) -> simplify (Until (f,g)) 
					| And(e,f), g when (f = g) -> simplify (Until (e,g)) 
					| e, And(f,g) when (f = e) -> simplify (Until (e,g)) 
					| e, And(f,g) when (g = e) -> simplify (Until (e,f)) 
					**)
					(* Removed the following two because of seed 9468 with bounded-existence pattern, may not be correct ? *)
					(* | And(e,f), g -> simplify  (And((Until(e,g)), (Until(f,g))))
					| e, Or(f,g) -> simplify (Or((Until(e,f)), (Until(e,g)))) *)
					| e, Until (f,g) when (e=f) -> Until (f,g)
					| e, f -> Until (e,f)
    )		
    | Iff (a, b) -> (
					match (simplify a), (simplify b) with
							True, e -> e
						| e, True -> e
						| False, e -> simplify (Neg e)
						| e, False -> simplify (Neg e)
						| e, Neg f when (e = f)  -> False	
						| e, f when (e= f) -> True
						| e,f -> simplify (And (Imp (e,f), Imp (f,e)))
    )
    | Imp (a, b) -> (
				match (simplify a), (simplify b) with
						False,_ -> True
					| _, True -> True
					| True, e -> e
					| e, False -> simplify (Neg e)
					| e, f when (e=f) -> True
					| e, f -> simplify (Or (Neg e, f))
    )
    | Ev True -> True
    | Ev False -> False
    | Ev Ev e -> simplify (Ev e)
    | Ev e -> (
				match (simplify e) with 
						True -> True
					| False -> False
					| And (f,Ev(And(g,h))) when ((f =g) || (f=h)) -> Ev (And (g,h))
					| And (Ev(And(g,h)),f) when ((f =g) || (f=h)) -> Ev (And (g,h))
					| Or(f,g) -> simplify (Or(Ev f,Ev (g)))
					| Ev f -> Ev f
					| f -> Ev f
    )
    | Glob e -> (
				match (simplify e) with 
						True -> True
					| False -> False
					| Glob f -> Glob f
					| Or (f,Glob (Or(g, h))) when ((f=g) || (f=h)) -> simplify (Glob (Or(g, h)))
					| And (f,Glob (And(g, h))) when ((f=g) || (f=h)) -> simplify (Glob f) 
					| And (f, g) -> simplify (And (Glob f, Glob g))
					| f -> Glob f
    )
		(* Maybe not correct ? *)
    | Next e -> (
				match (simplify e) with
						True -> True
					| False -> False
					| e -> Next e
		)
    (* Next e -> (match (simplify e) with 
       And (f,g) -> And(Next f,Next g)
       | Or (f,g) -> Or (Next f, Next g)
       | f -> Next f
       )
    *)
    | Previous  e -> (
				match (simplify e) with 
						And (f,g) -> simplify (And(Previous f,Previous g))
					| Or (f,g) -> simplify (Or (Previous f, Previous g))
					| f -> Previous f
    )
    | Wuntil (e,f) -> simplify (Or(Glob e, Until(e,f)))
    | SHARP -> SHARP
    | _ -> form


let simp = simplify

	  
let rec number_of_previous (form:ltl) =
	match form with
		Previous f -> 1+number_of_previous f
		| _ -> 0
	
(* Retrieve a formula nested with previous operator, i.e. retrieves Phi in X^m Phi 
It assumes the input formula to be in the form X^m Phi*)	
let rec retrieve_nested_formula_in_a_previous (form:ltl) =
	match form with
		Previous f -> retrieve_nested_formula_in_a_previous f
		| f -> f
	
let rec number_of_next (form:ltl) =
	match form with
		Next f -> 1+number_of_next f
		| _ -> 0
	
(* Retrieve a formula nested with previous operator, i.e. retrieves Phi in X^m Phi 
It assumes the input formula to be in the form X^m Phi*)	
let rec retrieve_nested_formula_in_a_next (form:ltl) =
	match form with
		Next f -> retrieve_nested_formula_in_a_next f
		| f -> f
		
let rec urgent_closure f =
	match  f with 
	 And (a, b) | Or (a,b) |  Iff (a,b) | Imp(a,b) | Xor (a,b) -> (urgent_closure a)@(urgent_closure b) 
    | Previous _ -> [f]
    | Neg a -> urgent_closure a
    | _ -> []


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
    | Previous a -> variables a
    | Var x -> [Var x]
    | Xor (a, b) -> (variables a) @ (variables b)
    | _ -> []
