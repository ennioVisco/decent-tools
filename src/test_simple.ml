open Batteries
open DynArray
open Alphabetevent
open Trace
open Dltl
open Architecture
open Common_test


let dalpha = [ ["a";"a1";"a2"] ; ["b";"b1";"b2"] ; ["c";"c1";"c2"]]
let alpha = globalAlphabet dalpha

let running_trace1 = [ ["a1";"b2"] ; ["a1";"b2"] ; ["a1";"b2"] ; ["b1";"a1";"c1"] ; ["a1";"b2"]]
let running_trace2 = [ ["a";"c"] ; ["a"; "b"] ; ["b"; "c"] ; ["a";"c"]]

let running_property1 = Until (Var "a1", And (Var "a1", And (Var "b1", Var "b2")))
let running_property2 = Until (And (Var "a", Var "b"), And (Var "a", And (Var "b", Var "c")))
let running_property3 =  Until (And (Var "a", Var "b"), Or (Var "c", And (Var "b", Var "a")))

let phi1 : dltl = Until (Var "b1", And (Or (Neg (And (Next (Var "b2"), Var "a1")), Var "a2"), Or (Neg (Var "a2"), And (Next (Var "b2"), Var "a1"))))
  
let phi2 : dltl = Ev (And (Or (Neg (Glob (Ev (Var "b1"))), Var "a1"), Or (Neg (Var "a1"), Glob (Ev (Var "b1")))))
                  
let phi3 : dltl = Or (Glob (Ev (Var "a1")), Neg (Glob (Ev (Var "b1"))))

let phi_pb : dltl = Ev (And (Or (Neg (Glob (Ev (Var "b1"))), Var "a1"), Or (Neg (Var "a1"), Glob (Ev (Var "b1")))))

let running_property4 = And (Var "c", Until (Var "a", And (Var "a", And (Var "b", Var "c"))))

let _ =
  Random.self_init();

  let trace2 = (gen_1_trace 10 alpha) in
  let phi = running_property4 and  trace = running_trace2 in
  
  if false then
    (
      let _ =  test_static_network phi dalpha in ()
    );
  if false then
    (
      test_dmonitoring_1trace_1formula dalpha phi trace
    );
  if false then
    (
      let _ = test_dmonitoring_1randomTrace_1randomFormula dalpha 5 10 in ()
    );
  if true then
    (
      test_correctness_monitoring_algo dalpha phi trace		   
    );
  if false then
    (
      test_correctness_monitoring_algo_1randomTrace_1Formula dalpha phi 10
    );
  if false then
    (
      test_correctness_monitoring_algo_XrandomTraces_1Formula dalpha phi 100 10
    );
  if false then
    (
      test_correctness_monitoring_algo_1randomTrace_1randomFormula dalpha 10 100
    );
  if false then
    (
      test_correctness_monitoring_algo_X_randomTraces_randomFormulae 1000 dalpha 6 100
    );

  print_endline("***END***")
