(* Copyright (c) 2011 Andreas Bauer <baueran@gmail.com>, Ylies Falcone <ylies.falcone@gmail.com> 
   Code is released under the GPL license version 3. *)
(* The following functions require Ocaml's preprocessor [camlp4]. *)

(*i*)
open Genlex
open Stream
(*i*)

(*s Bit of an ugly definition to make a lexer consisting of the
  keywords in the given list.  [lexer] is basically a function which
  takes an input stream and produces an output token stream. *)
  
let lexerTrace = 
  Genlex.make_lexer 
    ["{"; "}"; ";"; ","; "|"]

let rec parse_alphabet = parser
[< 'Kwd "{" ; alpha=parse_comma ; 'Kwd "}">] -> alpha
  
and parse_comma = parser
  | [< e1 = parse_atom; stream >] ->
    (parser  
      | [< 'Kwd ","; e2 = parse_comma >] -> [e1]@e2 
      | [< >] -> [e1] ) stream
  | [< >] -> []
    
and parse_atom = parser
  | [< 'Ident c ; stream >] -> c  
 	

let rec parse_dalphabet = parser
	[< 'Kwd "{" ; alpha=parse_mid ; 'Kwd "}">] -> alpha
	
	and parse_mid = parser
  | [< e1 = parse_comma; stream >] ->
      (parser  
      	| [< 'Kwd "|"; e2 = parse_mid >] -> [e1]@e2 
      	| [< >] -> [e1] ) stream
  | [< >] -> []
  
  	and parse_comma = parser
  | [< e1 = parse_atom; stream >] ->
      (parser  
      	| [< 'Kwd ","; e2 = parse_comma >] -> [e1]@e2 
      	| [< >] -> [e1] ) stream
  | [< >] -> []
  
(*s Function that invokes the alphabet parser on a string by streaming the input string and then "lexing" it   
*)
let parse_alphabet_string (alphabet_string:string) =
	parse_alphabet (lexerTrace (Stream.of_string alphabet_string))

(*s Function that invokes the distributed alphabet parser on a string by streaming the input string and then "lexing" it   
*)
let parse_dalphabet_string (dalphabet_string:string) =
	parse_dalphabet (lexerTrace (Stream.of_string dalphabet_string))

