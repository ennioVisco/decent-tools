(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com>, Ylies Falcone <ylies.falcone@gmail.com> 
   Code is released under the GPL license version 3. *)
(* The following functions require Ocaml's preprocessor [camlp4]. *)

(*i*)
open Genlex
open Stream
open Ltl
(**
open Printf
open Types

open Std
(*i*)
**)

(*s Bit of an ugly definition to make a lexer consisting of the
  keywords in the given list.  [lexer] is basically a function which
  takes an input stream and produces an output token stream. *)
  
let lexer = 
  Genlex.make_lexer 
    ["&"; "|"; "!"; "G"; "U"; "X"; "F"; "->"; "<->"; "True"; "False"; "("; ")"; "-"; "Y"; ","; "["; "]"]

(*s Definition of a recursive-descent stream parser. [parse_formula]
   is a function which takes a token stream and returns an LTL
   formula.  It is mutually recursive, giving rise to precedence of
   operators.  *)

let rec parse_atom = parser
  | [< 'Kwd "-"; e1 = parse_atom >] -> Neg (e1)
  | [< 'Ident c >] -> Var c
  | [< 'Kwd "("; e = parse_formula; 'Kwd ")" >] -> e

and parse_formula = parser
  | [< e1 = parse_and; stream >] -> 
      (parser 
	 | [< 'Kwd "|"; e2 = parse_formula >] -> Or (e1, e2)
	 | [< 'Kwd "U"; e2 = parse_formula >] -> Until (e1, e2)
	 | [< 'Kwd "->"; e2 = parse_formula >] -> Imp (e1, e2)
	 | [< 'Kwd "<->"; e2 = parse_formula >] -> Iff (e1, e2)
	 | [< >] -> e1) stream
      
and parse_and = parser
  | [< e1 = parse_atom; stream >] ->
      (parser
	 | [< 'Kwd "&"; e2 = parse_and >] -> And (e1, e2)
	 | [< >] -> e1) stream
  | [< e1 = parse_unary >] -> e1

and parse_unary = parser
  | [< 'Kwd "!"; e1 = parse_and >] -> Neg (e1)
  | [< 'Kwd "G"; e1 = parse_and >] -> Glob (e1)
  | [< 'Kwd "F"; e1 = parse_and >] -> Ev (e1)
  | [< 'Kwd "X"; e1 = parse_and >] -> Next (e1)
  | [< 'Kwd "Y"; e1 = parse_and >] -> Previous (e1)
  
let parse_formula_string formula_string =
	parse_formula (lexer (Stream.of_string formula_string))
  (**
let parse_formula_in_file file_name =
	List.map parse_formula_string (input_list (open_in file_name))
  **)	
let rec  parse_formulae_inside = parser
  | [< f1 = parse_formula; stream >] -> 
  	( parser 
  		 | [< 'Kwd "," ; f2 = parse_formulae_inside >] -> [f1]@f2
 		 | [< >] -> [f1] ) stream

and  parse_formulae = parser
 	[< 'Kwd "[" ; e=parse_formulae_inside ; 'Kwd "]">] -> e   

(* Parsing a formula encoded by a string *)

let parse_formulae_string formula_string =
	let formulae_list = parse_formulae (lexer (Stream.of_string formula_string)) in
		Array.of_list formulae_list

