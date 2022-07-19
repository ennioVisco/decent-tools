open Alphabetevent
open Ltl
open List
open Ltl_parser

(**
let gen_prop (alpha:alphabet) =
  let sub_alphabet = List.filter (fun x -> (Random.int 2)=1) alpha in
  if ((List.length sub_alphabet)=0) then False
  else
    parse_formula
      (lexer (Stream.of_string (
	fold_left (fun x y -> if (x="") then y
	  else (if ((Random.int 2)=1) then x^"&"^y else x^"|"^y)) "" sub_alphabet)	
       )
      )
**)


(*CC: biased gen_prop*)
let gen_prop_biased_component (d_alpha:d_alphabet) (comp:int)=
  let alpha = List.nth d_alpha (comp) in
  (*randomly select a subalphabet... aiming for a subalphabet of
    size 2*)
  let sub_alphabet = List.filter (fun x -> let value = (max (List.length alpha) 1 / 2) in (Random.int (max 1 value)=0)) alpha in
  (*AND/OR the chosen subalphabet*)
  if ((List.length sub_alphabet)=0) then
    False
  else
    parse_formula (lexer (Stream.of_string (
      fold_left (fun x y -> if (x="") then y
	else (if ((Random.int 2)=1) then x^"&"^y else x^"|"^y)) "" sub_alphabet)	
    ))


let gen_prop (alpha:alphabet) : ltl =
  let max = (List.length alpha) in
  let index = Random.int max in
  Var (nth alpha (index))

    
(* Generates randomly a future LTL formula - The generated formula may
   not be minimal and should be simplified before use. The boolean
   indicates whether the previous operator is allowed *)
    
let  gen_1_form_biased (size:int) (alpha:alphabet) (d_alpha:d_alphabet) (comp:int)  =
  let rec gen_1_form_biased (size:int) (alpha:alphabet)  =
    if size < 1
  then
    gen_prop_biased_component d_alpha comp
  else
    (   
      match (1+Random.int 11) with
   	  1 -> True
	| 2 -> False
	| 3 -> gen_prop (alpha)
	| 4 -> Or (gen_1_form_biased (size-1) alpha, gen_1_form_biased (size-1) alpha)
	| 5 -> And (gen_1_form_biased (size-1) alpha, gen_1_form_biased (size-1) alpha)
	| 6 -> Neg (gen_1_form_biased (size-1) alpha)
	| 7 -> Iff (gen_1_form_biased (size-1) alpha, gen_1_form_biased (size-1) alpha)
	| 8 -> Until (gen_1_form_biased (size-1) alpha, gen_1_form_biased (size-1) alpha)
	| 9 -> Next (gen_1_form_biased (size-1) alpha)
	| 10 -> Glob (gen_1_form_biased (size-1) alpha)
	| 11 -> Ev (gen_1_form_biased (size-1) alpha)
	| _ -> False
    )
  in gen_1_form_biased size alpha


let rec gen_1_form_nontrivial_biased (size:int) (alpha:d_alphabet) (comp:int): ltl =
  let form = simp (gen_1_form_biased size (globalAlphabet alpha) alpha comp) in
  if ((Ltl.size form) < (size / 2) || form = True || form = False) then
    gen_1_form_nontrivial_biased size alpha comp
  else
    form
  
(* Generates randomly a future LTL formula - The generated formula may
   not be minimal and should be simplified before use. The boolean
   indicates whether the previous operator is allowed *)
    
let rec gen_1_form (size:int) (alpha:alphabet)  =
  if size < 1
  then
    gen_prop alpha
  else
    (   
      match (1+Random.int 11) with
   	  1 -> True
	| 2 -> False
	| 3 -> gen_prop (alpha)
	| 4 -> Or (gen_1_form (size-1) alpha, gen_1_form (size-1) alpha)
	| 5 -> And (gen_1_form (size-1) alpha, gen_1_form (size-1) alpha)
	| 6 -> Neg (gen_1_form (size-1) alpha)
	| 7 -> Iff (gen_1_form (size-1) alpha, gen_1_form (size-1) alpha)
	| 8 -> Until (gen_1_form (size-1) alpha, gen_1_form (size-1) alpha)
	| 9 -> Next (gen_1_form (size-1) alpha)
	| 10 -> Glob (gen_1_form (size-1) alpha)
	| 11 -> Ev (gen_1_form (size-1) alpha)
	| _ -> False
    )
      
let rec gen_1_form_nontrivial (size:int) (alpha:alphabet) : ltl =
  let form = simp (gen_1_form size alpha) in
  if ((Ltl.size form) < (size / 2) || form = True || form = False) then
    gen_1_form_nontrivial size alpha
  else
    form
	
let rec generate_array_formulae_same (num:int) (size_max:int) (alpha: alphabet) =
  if (num=0) then
    raise Error
  else
    let form= (gen_1_form (Random.int size_max) alpha) in
    Array.make num form
		
		
(*
Absence

P is false :
Before R 	<>R -> (!P U R)
After Q 	[](Q -> [](!P))
Between Q and R 	[]((Q & !R & <>R) -> (!P U R))
After Q until R 	[](Q & !R -> (!P W R))
Globally 	[](!P)
*)	
			
let print_form (form: ltl) =
	print_endline (string_rep form); 
	print_endline (string_rep_simple form); 
	print_endline (string_rep_tree form); 
	simp form

let rec gen_1_form_abscence (alpha:alphabet) =
	let f (alpha:alphabet) =
	let p = gen_prop (alpha) and q = gen_prop(alpha) and r = gen_prop (alpha) in
	match (Random.int 5) with
		 1 -> Imp (Ev r , (Until (Neg p, r)))
		| 2 -> 	Glob (Imp (q, Glob (Neg r)))
		| 3 -> 	Glob (Imp(And (q,And(Neg r,Ev r)), Until(Neg p,r)))
		| 4 ->	Glob (Imp(And(q,Neg r),Wuntil(Neg p,r)))
		| _ -> Glob (Neg r)
	in let form = simp(f(alpha)) in
			 if (form= True || form= False) then gen_1_form_abscence(alpha)
				else form
		
(*
Existence

P becomes true :
Globally 	<>(P)
Before R 	!R W (P & !R)
After Q 	[](!Q) | <>(Q & <>P))
Between Q and R 	[](Q & !R -> (!R W (P & !R)))
After Q until R 	[](Q & !R -> (!R U (P & !R)))
*)	
	
let rec gen_1_form_existence (alpha:alphabet) =
let f (alpha:alphabet) =
	let p = gen_prop (alpha) and q = gen_prop(alpha) and r = gen_prop (alpha) in
	match (Random.int 5) with 
		1 -> Ev (p)
		| 2 -> Wuntil (Neg r, And(p,Neg r))
		| 3 -> Or(Glob (Neg q), Ev (And(q, Ev p)))
		| 4 -> Glob (Imp (And(q,Neg r),Wuntil (Neg r,And(p,Neg r))))
		| _ -> Glob (Imp (And(q,Neg r),Until(Neg r, And(p,Neg r))))
		in let form = simp(f(alpha)) in
			 if (form= True || form= False) then gen_1_form_existence(alpha)
				else form
				
				
(*
Bounded Existence

Globally 	(!P W (P W (!P W (P W []!P))))
Before R 	

<>R -> ((!P & !R) U (R | ((P & !R) U
         (R | ((!P & !R) U (R | ((P & !R) U
            (R | (!P U R)))))))))

After Q 	<>Q -> (!Q U (Q & (!P W (P W (!P W (P W []!P))))))
Between Q and R 	

[]((Q & <>R) ->
   ((!P & !R) U (R | ((P & !R) U
     (R | ((!P & !R) U (R | ((P & !R) U
       (R | (!P U R))))))))))

After Q until R 	

[](Q -> ((!P & !R) U (R | ((P & !R) U
          (R | ((!P & !R) U (R | ((P & !R) U
            (R | (!P W R) | []P)))))))))
*)

let rec gen_1_form_boundedexistence (alpha:alphabet) =
let f (alpha:alphabet) =
	let p = gen_prop (alpha) and q = gen_prop(alpha) and r = gen_prop (alpha) in
	match (Random.int 5) with 
		1 -> let form11 = Wuntil (Neg p, Wuntil (p, Glob (Neg p))) in let form1 = Wuntil (p, form11) in Wuntil (Neg p, form1) 
		
		| 2 -> let form1 = And(Neg p, Neg r) and form2 = And (p, Neg r) and form3 = Until (Neg p, r) in let form41 = Or(r,Until (form1, Or(r,Until (form2,Or(r,form3))) )) in let form4 =  Until (form2,form41) in
				 Imp (Ev r, Until(form1,Or(r,form4)))
				
		| 3 -> Imp (Ev q, Until (Neg q, And(q, Wuntil (Neg p, Wuntil(p, Wuntil (Neg p, Wuntil (p, Glob (Neg p))))))))
		
		| 4 -> let forma = And (Neg p, Neg r) and formb = And (p,Neg r) in let form111 = Or(r, Until (formb, Or (r, Until (Neg p, r)))) in let form11 = Or (r,Until (forma, Or (r, form111))) in let form1 = Until(forma,form11) in Glob (Imp(And(q,Ev r),form1))
		
		
		| _ -> let forma = And (Neg p, Neg r) and formb = And (p,Neg r) in let form11 = Or (r, Until (formb, Or (r, Or (Wuntil (Neg p, r),Glob p)))) in let form1 = Until (forma, Or (r,Until (formb, Or (r, Until (forma, form11))))) in Glob (Imp (q,form1))
		
		in let form = simp(f(alpha)) in
			 if (form= True || form= False) then gen_1_form_boundedexistence(alpha)
				else form


(*
Universality

P is true :
Globally 	[](P)
Before R 	<>R -> (P U R)
After Q 	[](Q -> [](P))
Between Q and R 	[]((Q & !R & <>R) -> (P U R))
After Q until R 	[](Q & !R -> (P W R))
*)	

let rec gen_1_form_universality (alpha:alphabet) =
let f (alpha:alphabet) =
	let p = gen_prop (alpha) and q = gen_prop(alpha) and r = gen_prop (alpha) in
	match (Random.int 5) with 
		1 -> Glob p
		| 2 -> Imp (Ev r, Until (p,r))
		| 3 -> Glob (Imp(q, Glob p))
		| 4 -> Glob ( Imp (And (q,And(Neg r,Ev r)),Until (p,r)) )
		| _ -> Glob (Imp(And(q,Neg r),Wuntil(p,r)))
		
		in let form = simp(f(alpha)) in
			 if (form= True || form= False) then gen_1_form_universality(alpha)
				else form

(*
Precedence

S precedes 
 Globally 	!P W S
Before R 	<>R -> (!P U (S | R))
After Q 	[]!Q | <>(Q & (!P W S))
Between Q and R 	[]((Q & !R & <>R) -> (!P U (S | R)))
After Q until R 	[](Q & !R -> (!P W (S | R)))
*)

let rec gen_1_form_precedence (alpha:alphabet) =
let f (alpha:alphabet) =
	let p = gen_prop (alpha) and q = gen_prop(alpha) and r = gen_prop (alpha) and s = gen_prop (alpha) in
	match (Random.int 5) with 
		1 -> Wuntil (Neg p, s)
		| 2 -> Imp (Ev r, Until (Neg p, Or (s, r)))
		| 3 -> Or (Glob( (Neg q)), Ev (And (q, Wuntil (Neg p,s))))
		| 4 -> Glob (Imp (And (q, And(Neg r, Ev r)),Until(Neg p, Or (s, r))))
		| _ -> Glob (Imp (And (q, Neg r), Wuntil (Neg p, Or (s,r))))
		
		in let form = simp (f(alpha)) in
			 if (form= True || form= False) then gen_1_form_precedence(alpha)
				else form


(*
Response

S responds to P :
Globally 	[](P -> <>S)
Before R 	<>R -> (P -> (!R U (S & !R))) U R
After Q 	[](Q -> [](P -> <>S))
Between Q and R 	[]((Q & !R & <>R) -> (P -> (!R U (S & !R))) U R)
After Q until R 	[](Q & !R -> ((P -> (!R U (S & !R))) W R)
*)

let rec gen_1_form_response (alpha:alphabet) =
	let f (alpha:alphabet) =
	let p = gen_prop (alpha) and q = gen_prop(alpha) and r = gen_prop (alpha) and s = gen_prop (alpha) in
		match (Random.int 5) with 
			1 -> Glob (Imp (p,Ev s))
			| 2 -> let form2 = Until (Neg r, And (s, Neg r)) in let form1 = Imp (p, form2) in let form = Until (form1,r)
					in Imp (Ev r, form)
			| 3 -> Glob ( Imp (q, Glob (Imp(p,Ev s))))
			| 4 -> let form121 = Until (Neg r, And (s, Neg r)) in let form11 = And(q, And (Neg r, Ev r)) and form12 = Imp (p, form121)in let form1 = Imp (form11,form12) in Glob (Until (form1, r))
			| _ -> let form2 = Imp (p, Until (Neg r, And (s, Neg r))) in let form1 = Wuntil (form2,r) in let form = Imp ( And (q, Neg r), form1) in Glob (form) 
			
			in let form = simp(f(alpha)) in
			 if (form= True || form= False) then gen_1_form_response(alpha)
				else form
			
(*
Precedence Chain

This illustrates the 2 cause-1 effect precedence chain.

S, T precedes P:
1/ Globally 	<>P -> (!P U (S & !P & o(!P U T)))
2/ Before R 	<>R -> (!P U (R | (S & !P & o(!P U T))))
3/ After Q 	([]!Q) | (!Q U (Q & <>P -> (!P U (S & !P & o(!P U T))))
4/ Between Q and R 	[]((Q & <>R) -> (!P U (R | (S & !P & o(!P U T)))))
5/ After Q until R 	[](Q -> (<>P -> (!P U (R | (S & !P & o(!P U T))))))

This illustrates the 1 cause-2 effect precedence chain.

P precedes (S, T):
6/ Globally 	(<>(S & o<>T)) -> ((!S) U P))
7/ Before R 	<>R -> ( (!(S & (!R) & o(!R U (T & !R)))) U (R | P) )
8/ After Q 	([]!Q) | ((!Q) U (Q & ((<>(S & o<>T)) -> ((!S) U P)))
9/ Between Q and R 	[]((Q & <>R) -> ((!(S & (!R) & o(!R U (T & !R)))) U (R | P)))
10 (-) / After Q until R 	[](Q -> (!(S & (!R) & o(!R U (T & !R))) U (R | P) | [](!(S & o<>T)))) 

*)


let rec gen_1_form_precedence_chain (alpha:alphabet) =
		let f (alpha:alphabet) =
		let p = gen_prop (alpha) and q = gen_prop(alpha) and r = gen_prop (alpha) and s = gen_prop (alpha) and t = gen_prop (alpha) in
		match (Random.int 10) with
			1 -> let form1 = And (s, And (Neg p, Next (Until(Neg p, t)))) in let form = Until (Neg p , form1) in Imp (Ev p , form)
		  | 2 -> let form2 = Or (r, And (s, And (Neg p, Next (Until (Neg p, t))))) in let form1 = Or (r, form2) in let form = Until (Neg p , form1) in Imp (Ev r , form)
		  | 3 -> let form121 = Or (r, And (s,And(Neg p, Next(Until(Neg p,t))))) in let form11 = And(q, Ev p) and form12 = Until (Neg p, form121) in let form1 = Imp(form11,form12) in let form = Until (Neg q, form1) in Or(Glob (Neg q),form)
		  | 4 -> let form21 = Neg p and form22 = Or (r, And (s, And (Neg p, Next(Until(Neg p,t))))) in let form1 = And(q,Ev r) and form2 = Until (form21,form22) in Glob (Imp(form1,form2))
		  | 5 -> let form2111 = And (s, And (Neg p, Next (Until (Neg p, t)))) in let form211 = Or (r, form2111) in let form21 = Until (Neg p, form211) in let form1 = q and form2 = Imp(Ev p, form21) in Glob (Imp(form1, form2))
		  | 6 ->  let form1 = Ev (And (s, Next(Ev t))) and form2 = Until (Neg s, p) in Imp (form1,form2)
		  | 7 -> let form211 = Next(Until (Neg r, And (t,Neg r))) in let form21 = Neg(And (s, And (Neg r, form211))) in let form1 = (Ev r) and form2 = Until (form21, Or (r,p)) in Imp (form1,form2)
		  | 8 -> let form111 = Imp (Ev (And (s,Next(Ev t))),Until (Neg s,p)) in let form11 = And (q, form111) in let form1 = Until (Neg q, form11) in Or (Glob (Neg q), form1)
		  | 9 -> let form211 = Next (Until (Neg r,And (t,Neg r))) in let form21 = Neg (And (s, And (Neg r, form211))) in let form1 = And(q, Ev r) and form2 = Until(form21, Or (r,p)) in Glob (Imp(form1,form2))
		| _ -> let form11 = Neg (And(s,And(Neg r,Next(Until(Neg r,And(t,Neg r)))))) and form12 = Or(Or(r,p), Glob (Neg(And(s,Next(Ev t))))) in let form1 = Until (form11,form12) in Glob (Imp (q,form1))
	
		in let form = simp(f(alpha)) in
			 if (form= True || form= False) then gen_1_form_precedence_chain(alpha)
				else form
(*
Response Chain

This illustrates the 2 stimulus-1 response chain.

P responds to S,T:
1/Globally 	[] (S & o<> T -> o(<>(T & <> P)))
2/Before R 	<>R -> (S & o(!R U T) -> o(!R U (T & <> P))) U R
3/After Q 	[] (Q -> [] (S & o<> T -> o(!T U (T & <> P))))
4/Between Q and R 	[] ((Q & <>R) -> (S & o(!R U T) -> o(!R U (T & <> P))) U R)
5/After Q until R 	

[] (Q -> (S & o(!R U T) -> o(!R U (T & <> P))) U
    (R | [] (S & o(!R U T) -> o(!R U (T & <> P)))))

This illustrates the 1 stimulus-2 response chain.

S,T responds to P:
6/Globally 	[] (P -> <>(S & o<>T))
7/Before R 	<>R -> (P -> (!R U (S & !R & o(!R U T))) ) U R
8/After Q 	[] (Q -> [] (P -> (S & o<> T)))
9/Between Q and R 	[] ((Q & <>R) -> (P -> (!R U (S & !R & o(!R U T)))) U R)
10 (_))After Q until R 	

[] (Q -> (P -> (!R U (S & !R & o(!R U T)))) U
    (R | [] (P -> (S & o<> T))))
 *)



let rec gen_1_form_response_chain  (alpha:alphabet) =
	let f (alpha:alphabet) =

	let p = gen_prop (alpha) and q = gen_prop(alpha) and r = gen_prop (alpha) and s = gen_prop (alpha) and t = gen_prop (alpha) in
		match (Random.int 10) with
		1 -> let form1 = And (s, Next(Ev t)) and form2 = Next(Ev(And(t,Ev p))) in Glob (Imp (form1,form2))
		| 2-> let form = Imp (And (s,Next(Until(Neg r,t))), Next(Until(Neg r,And(t, Ev p)))) in Imp(Ev r, Until(form, r))
		| 3 -> let form1 = Imp(And(s,Next(Ev t)), Next(Until (Neg t,And(t,Ev p)))) in Glob (Imp(q, Glob(form1)))
		| 4 -> let form21 = Imp (And(s,Next(Until (Neg r,t))), Next(Until(Neg r,And(t,Ev p)))) in let form1 = And(q,Ev r) and form2 = Until(form21,r) in Glob (Imp (form1, form2))
		| 5 -> let form21 = And(s, Next(Until(Neg r, t))) and form22 = Next(Until(Neg r, And (t,Ev p))) in let form1 = Imp(And(q,Ev r),Next (Until(Neg r,And(t, Ev p)))) and form2 = Or (r, Glob (Imp(form21,form22))) in Glob (Imp(q, Until(form1,form2)))
		| 6 -> Glob (Imp(p,Ev (And(s,Next t))))
		| 7 -> let form2 = Until (Neg r, And(s,And(Neg r,Next(Until(Neg r,t))))) in let form1 = Imp (p, form2) in let form = Until (form1, r) in Imp(Ev r, form)
		| 8 -> Glob (Imp(q,Glob(Imp(p,And(s,Next(Ev t))))))
		| 9 -> let form211 = And (s,And(Neg r,Next(Until (Neg r, t)))) in let form21 = Until(Until(Neg r,form211), r) in let form1 = And(q,Ev r) and form2 = Until(Imp(p,form21),r) in Glob (Imp(form1,form2))
		| _ -> let form11 = Until (Neg r, And(s, And(Neg r, Next(Until(Neg r, t))))) in let form1 = Imp (p,form11) and form2 = Or(r,Glob(Imp(p,And(s,Next(Ev t))))) in Glob (Imp(q,Until (form1,form2)))
		
				in let form = simp (f(alpha)) in
			 if (form= True || form= False) then gen_1_form_response_chain(alpha)
				else form
				
(*
Constrained Chain Patterns

This is the 1-2 response chain constrained by a single proposition.

S,T without Z responds to P: 

1/Globally 	[] (P -> <>(S & !Z & o(!Z U T)))
2/Before R 	<>R -> (P -> (!R U (S & !R & !Z & o((!R & !Z) U T)))) U R
3/After Q 	[] (Q -> [] (P -> (S & !Z & o(!Z U T))))
4/Between Q and R 	[] ((Q & <>R) -> (P -> (!R U (S & !R & !Z & o((!R & !Z) U T)))) U R)
5/After Q until R 	

[] (Q -> (P -> (!R U (S & !R & !Z & o((!R & !Z) U T)))) U
    (R | [] (P -> (S & !Z & o(!Z U T)))))

*)				
let rec gen_1_form_constrained_chain  (alpha:alphabet) =
	let f (alpha:alphabet) =

	let p = gen_prop (alpha) and t = gen_prop(alpha) and z = gen_prop (alpha) and r = gen_prop (alpha) and q = gen_prop (alpha) and s = gen_prop (alpha) in
		match (Random.int 5) with
		1 -> let form1 = And (s,And(Neg z, Next (Until(Neg z,t)))) in Glob (Imp (p, Ev form1))
		| 2 -> let form4 = And (s, And(Neg r,And(Neg z,Next(Until(Neg z,t))))) in let form3 = Until(Neg r,form4) in let form2 = Imp(p, form3) in let form1 = Imp (Ev r, form2) in Until (form1, r)
		| 3 -> let form2 = And(s, And (Neg z, Next(Until(Neg z,t)))) in let form1 = Imp (p,form2) in Glob (Imp(q,Glob form1))
		| 4 -> let form4 = And (s,And(Neg r, And(Neg z, Next (Until (And (Neg r, Neg z),t))))) in let form3 = Until (Neg r,form4) in let form2 = Imp (p,form3) in let form1 = Imp (And(q,Ev r), form2) in Glob (Until(form1,r))
		
		| _ -> let form12 = And(s,And(Neg z,Next(Until(And(Neg r,Neg t),t)))) in let form11 = Imp (p, form12) and form21 = Imp (p,And(s,And(Neg z, Next(Until (Neg z,t))))) in let form1 = Imp (q,form11) and form2 = Or (r,Glob (form21)) in Glob (Until (form1,form2)) 
		
			in let form = simp(f(alpha)) in
			 if (form= True || form= False) then gen_1_form_constrained_chain(alpha)
				else form
		
		
