open Alphabetevent
open Ltl
open Utils
exception Error

let cprogress (form:ltl) (e:event)  =
  let f = simp form in
  let rec progress_rec f =
    match simp f with 
	False -> False
      | True -> True
      | Var p -> if (List.mem p e) then True else False
      | Or (f1,f2) -> Or(progress_rec f1, progress_rec f2)
      | And(f1,f2) -> And(progress_rec f1, progress_rec f2)
      | Neg(f1) -> Neg (progress_rec f1)
      | Next(f1) -> f1
      | Until (f1,f2) -> Or(progress_rec f2, And(progress_rec f1, Until (f1,f2)))
      | Glob (f1) -> And(progress_rec f1, Glob f1)
      | Ev(f1) -> Or (progress_rec f1, Ev f1)
      | SHARP -> SHARP
      | Xor (f1,f2) -> Xor (progress_rec f1, progress_rec f2)
      | _ -> False 	
  in progress_rec f
