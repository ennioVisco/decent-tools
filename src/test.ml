open Batteries
open DynArray
open Alphabetevent
open Alphabet_parser
open Trace
open Dltl
open Ltl_parser
open Architecture
open Common_test
open Config
open Headers
open Utils

let nbtests = ref 0
let sizeform = ref 0
let maxsizeform = ref 0
let sizetrace = ref 0
let dalphabet_string = ref ""
let dalphabet = ref []
let alphabet_string = ref ""
let alphabet = ref []
let abscence = ref false
let existence = ref false
let bexistence = ref false
let universality = ref false
let response = ref false
let precedence_chain = ref false
let precedence = ref false
let response_chain= ref false
let constrained_chain= ref false
let bias = ref false
let precision = ref 3
let eval = ref false
let file_name = ref ""
let keep_samples = ref false
let nb_samples = ref 0
let counter_tests = ref 0
let counter_samples = ref 0
  
let iteration_counter = ref 0
  
let cent_trace_len = ref 0 and decent_trace_len = ref 0 and odecent_trace_len = ref 0 and cent_num_mess = ref 0 and decent_num_mess = ref 0 and odecent_num_mess = ref 0 and cent_size_mess = ref 0. and decent_size_mess = ref 0. and odecent_size_mess = ref 0. and cent_nb_progressions = ref 0  and decent_nb_progressions = ref 0 and odecent_nb_progressions = ref 0

let cent_trace_len_avg = ref 0. and decent_trace_len_avg = ref 0. and odecent_trace_len_avg = ref 0. and cent_num_mess_avg = ref 0. and decent_num_mess_avg = ref 0. and odecent_num_mess_avg = ref 0. and cent_size_mess_avg = ref 0. and decent_size_mess_avg = ref 0. and odecent_size_mess_avg = ref 0. and cent_nb_progressions_avg = ref 0.  and decent_nb_progressions_avg = ref 0. and odecent_nb_progressions_avg = ref 0. and delay_avg = ref 0. and odelay_avg = ref 0.

let cent_trace_len_var = ref 0. and decent_trace_len_var = ref 0. and odecent_trace_len_var = ref 0. and cent_num_mess_var = ref 0. and decent_num_mess_var = ref 0. and odecent_num_mess_var = ref 0. and cent_size_mess_var = ref 0. and decent_size_mess_var = ref 0. and odecent_size_mess_var = ref 0. and cent_nb_progressions_var = ref 0.  and decent_nb_progressions_var = ref 0. and odecent_nb_progressions_var = ref 0. and delay_var = ref 0. and odelay_var = ref 0.

let cent_num_mess_trin = ref 0. and decent_num_mess_trin = ref 0. and odecent_num_mess_trin = ref 0. and cent_size_mess_trin = ref 0. and decent_size_mess_trin = ref 0. and odecent_size_mess_trin = ref 0. and cent_nb_progressions_trin = ref 0.  and decent_nb_progressions_trin = ref 0. and odecent_nb_progressions_trin = ref 0. 



let delay = ref 0 and odelay = ref 0
let formula = ref False
 
let max_delay = ref 0 and max_odelay = ref 0
 
let rec gen_space (n:int) =
  if (n<=0) then ""
  else " "^gen_space(n-1)
	
let gen_cell (s:string) (n:int)=
  gen_space((n-(String.length s))/2)^s^gen_space((n-(String.length s))/2)

let adjust_string (s:string) (n:int) =
  if (String.length s = n) then s
  else (if (String.length s <= n) then s^(gen_space(n-(String.length s))) else s)

let prepare_display (s:string) (n:int) : string =
  adjust_string (gen_cell s n) n
    
let usage = "usage: " ^ Sys.argv.(0) ^ " [-n int]"
 
let speclist = [
  ("-n", Arg.Int    (fun n -> nbtests := n),  ": the number of tests to run");
  ("-sf", Arg.Int    (fun sf -> sizeform := sf),      ": the size of the formula");
  ("-msf", Arg.Int    (fun msf -> maxsizeform := msf),      ": the maximum size of the formula (will test from size 1 to the value provided)");
  ("-st", Arg.Int    (fun st -> sizetrace := st),      ": the size of the trace");
  ("-dalpha", Arg.String  (fun dalpha -> dalphabet_string := dalpha),     ": the decentralized alphabet");
  ("-alpha", Arg.String  (fun alpha -> alphabet_string := alpha),     ": the centralized alphabet (will consider possible dalphabets generated from it)");
  ("-abs", Arg.Bool  (fun abs -> abscence := abs),     ": testing for abscence patterns");
  ("-exis", Arg.Bool  (fun exis -> existence := exis),     ": testing for existence patterns");
  ("-bexis", Arg.Bool  (fun bexis -> bexistence := bexis),     ": testing for bounded existence patterns");
  ("-univ", Arg.Bool  (fun univ -> universality := univ),     ": testing for universality patterns");
  ("-prec", Arg.Bool  (fun prec -> precedence := prec),     ": testing for precedence patterns");
  ("-resp", Arg.Bool  (fun resp -> response := resp),     ": testing for response patterns");
  ("-precc", Arg.Bool  (fun precc -> precedence_chain := precc),     ": testing for precedence chain patterns");
  ("-respc", Arg.Bool  (fun respc -> response_chain:= respc),     ": testing for response chain patterns");
  ("-consc", Arg.Bool  (fun consc -> constrained_chain:= consc),     ": testing for constrained chain patterns");
  ("-prt_trace_mess", Arg.Bool  (fun prttracemess -> print_trace_and_mess_stats := prttracemess),     ": printing trace and number of messages statistics");
  ("-prt_delay", Arg.Bool  (fun prtdelay -> print_delay_stats := prtdelay),     ": printing delay statistics");
  ("-prt_full", Arg.Bool  (fun prtfull -> print_full_stats := prtfull),     ": printing full statistics");
  ("-flipcoin", Arg.Unit (fun x -> Trace.the_distrib := FLIPCOIN), "Using the flipcoin probability distribution (uniform distribution with probabily 0.5)");
  ("-bernouilli", Arg.Float (fun seed -> Trace.the_distrib := BERNOUILLI; Trace.seed := seed), "Using the BERNOUILLI probability distribution (uniform distribution with a probability given as an argument)");
  ("-expo", Arg.Float (fun seed -> Trace.the_distrib := EXPO; Trace.seed := seed), "Using the EXPONENTIAL probability distribution (the rate parameter is given as an argument)");
  ("-beta", Arg.Tuple [Arg.Float (fun seed -> Trace.the_distrib := BETA; Trace.seeda := seed);
    		       Arg.Float (fun seed -> Trace.seedb := seed)],
   "Using the BETA probability distribution (the rate parameters are given as arguments)");
  
  ("-only_changes", Arg.Unit  (fun x -> Config.mode := SEND_CHANGES),": components send the value of their propositions only if there is a change in its value. More precisely, if among the monitors, there is atleast the value of one proposition that is modified wrt the previous event, the component has to send a message to the monitor");
  ("-bias", Arg.Unit    (fun _ -> bias := true),  ": bias the generation of formulae to favor one component; the integer parameter is the index of the component");
  ("-precision", Arg.Int    (fun p -> precision := p),  ": precision of numbners (number of decimals)");
  ("-eval", Arg.Unit    (fun p -> eval := true),  ": CURRENT EVAL)");
  ("-keep_samples", Arg.Bool  (fun keep -> keep_samples := keep),     "keeping samples");
 ("-file", Arg.String  (fun file -> file_name := file),     ": the name of the file on which samples should be stored");
 ("-nb_samples", Arg.Int  (fun nb -> nb_samples := nb),     ": the number of samples to obtain");

]

let round_generic (f:float) (precision : int) : float =
  let candidate = (floor (f *. (10. ** float_of_int precision))) /. (10. ** float_of_int precision) in
  if candidate > 10000. then
    floor (candidate)
  else
    (
      if (candidate >= 1000.) then
	floor (candidate *. 100.) /. 100.
      else
	candidate
    )
  
let round (f:float) : float = round_generic f !precision 

let round_var (f:float) : float = round_generic f 2

  
let variance (sum_square:float) (avg:float) (size:int) =
  round_var ( ((sum_square /. (float_of_int size)) -. (avg**2.))**(1./.2.))

let compute_avg (the_list: 'a list) =
  let dalpha_length_tmp = ref 0 and
      net_depth_tmp = ref 0 and
      cent_trace_len_tmp = ref 0 and
      decent_trace_len_tmp = ref 0 and
      odecent_trace_len_tmp = ref 0 and
      cent_num_mess_tmp = ref 0 and
      decent_num_mess_tmp = ref 0 and
      odecent_num_mess_tmp = ref 0 and
      cent_size_mess_tmp = ref 0. and
      decent_size_mess_tmp = ref 0. and
      odecent_size_mess_tmp = ref 0. and
      cent_nb_progressions_tmp = ref 0 and
      decent_nb_progressions_tmp = ref 0 and
      odecent_nb_progressions_tmp = ref 0 in
  let summing (_, dalpha_length, net_depth, cent_trace_len, decent_trace_len, odecent_trace_len, cent_num_mess, decent_num_mess, odecent_num_mess, cent_size_mess, decent_size_mess, odecent_size_mess, cent_nb_progressions, decent_nb_progressions, odecent_nb_progressions) : unit =
    dalpha_length_tmp := !dalpha_length_tmp + dalpha_length;
    net_depth_tmp := !net_depth_tmp + net_depth;
    cent_trace_len_tmp := !cent_trace_len_tmp + cent_trace_len ;
    decent_trace_len_tmp := !decent_trace_len_tmp + decent_trace_len  ;
    odecent_trace_len_tmp := !odecent_trace_len_tmp + odecent_trace_len;
    cent_num_mess_tmp := !cent_num_mess_tmp + cent_num_mess;
    decent_num_mess_tmp := !decent_num_mess_tmp + decent_num_mess;
    odecent_num_mess_tmp := !odecent_num_mess_tmp + odecent_num_mess;
    cent_size_mess_tmp := !cent_size_mess_tmp +. cent_size_mess;
    decent_size_mess_tmp := !decent_size_mess_tmp +. decent_size_mess;
    odecent_size_mess_tmp := !odecent_size_mess_tmp +. odecent_size_mess;
    cent_nb_progressions_tmp := !cent_nb_progressions_tmp + cent_nb_progressions;
    decent_nb_progressions_tmp := !decent_nb_progressions_tmp + decent_nb_progressions;
    odecent_nb_progressions_tmp := !odecent_nb_progressions_tmp + odecent_nb_progressions in
  List.iter summing the_list;
  let size = float_of_int (List.length the_list) in
  let dalpha_length_avg = float_of_int !dalpha_length_tmp /. size and
  net_depth_avg = float_of_int !net_depth_tmp  /. size and
  cent_trace_len_avg = float_of_int !cent_trace_len_tmp /. size and
  decent_trace_len_avg = float_of_int !decent_trace_len_tmp    /. size and
  odecent_trace_len_avg = float_of_int !odecent_trace_len_tmp  /. size and
  cent_num_mess_avg =  float_of_int !cent_num_mess_tmp  /. size and
  decent_num_mess_avg = float_of_int !decent_num_mess_tmp  /. size and
  odecent_num_mess_avg = float_of_int !odecent_num_mess_tmp  /. size and
  cent_size_mess_avg = !cent_size_mess_tmp  /. size and
  decent_size_mess_avg = !decent_size_mess_tmp  /. size and
  odecent_size_mess_avg =  !odecent_size_mess_tmp  /. size and
  cent_nb_progressions_avg =  float_of_int !cent_nb_progressions_tmp  /. size and
  decent_nb_progressions_avg =  float_of_int !decent_nb_progressions_tmp  /. size and
  odecent_nb_progressions_avg =  float_of_int !odecent_nb_progressions_tmp  /. size in

    (dalpha_length_avg,
     net_depth_avg,
     size,
     cent_trace_len_avg,
     decent_trace_len_avg,
     odecent_trace_len_avg, 
     cent_num_mess_avg,
     decent_num_mess_avg,
     odecent_num_mess_avg,
     cent_size_mess_avg,
     decent_size_mess_avg,
     odecent_size_mess_avg,
     cent_nb_progressions_avg,
     decent_nb_progressions_avg,
     odecent_nb_progressions_avg)
    
      
let compute_var (the_list: 'a list) =
  let (dalpha_length_avg, net_depth_avg, size, cent_trace_len_avg, decent_trace_len_avg, odecent_trace_len_avg, cent_num_mess_avg, decent_num_mess_avg, odecent_num_mess_avg,
       cent_size_mess_avg, decent_size_mess_avg, odecent_size_mess_avg, cent_nb_progressions_avg, decent_nb_progressions_avg, odecent_nb_progressions_avg) = compute_avg the_list in
  let dalpha_length_tmp = ref 0. and
      net_depth_tmp = ref 0. and
      cent_trace_len_tmp = ref 0. and
      decent_trace_len_tmp = ref 0. and
      odecent_trace_len_tmp = ref 0. and
      cent_num_mess_tmp = ref 0. and
      decent_num_mess_tmp = ref 0. and
      odecent_num_mess_tmp = ref 0. and
      cent_size_mess_tmp = ref 0. and
      decent_size_mess_tmp = ref 0. and
      odecent_size_mess_tmp = ref 0. and
      cent_nb_progressions_tmp = ref 0. and
      decent_nb_progressions_tmp = ref 0. and
      odecent_nb_progressions_tmp = ref 0. in
  let summing_square (_, dalpha_length, net_depth, cent_trace_len, decent_trace_len, odecent_trace_len, cent_num_mess, decent_num_mess, odecent_num_mess, cent_size_mess, decent_size_mess, odecent_size_mess, cent_nb_progressions, decent_nb_progressions, odecent_nb_progressions) : unit =
    dalpha_length_tmp := !dalpha_length_tmp +. (float_of_int dalpha_length) ** 2. ;
    net_depth_tmp := !net_depth_tmp +. (float_of_int net_depth) ** 2.;
    cent_trace_len_tmp := !cent_trace_len_tmp +. (float_of_int cent_trace_len) ** 2. ;
    decent_trace_len_tmp := !decent_trace_len_tmp +. (float_of_int decent_trace_len)  ** 2.  ;
    odecent_trace_len_tmp := !odecent_trace_len_tmp +. (float_of_int odecent_trace_len) ** 2.;
    cent_num_mess_tmp := !cent_num_mess_tmp +. (float_of_int cent_num_mess) ** 2. ;
    decent_num_mess_tmp := !decent_num_mess_tmp +. (float_of_int decent_num_mess) ** 2.;
    odecent_num_mess_tmp := !odecent_num_mess_tmp +. (float_of_int odecent_num_mess) ** 2.;
    cent_size_mess_tmp := !cent_size_mess_tmp +. cent_size_mess ** 2.;
    decent_size_mess_tmp := !decent_size_mess_tmp +. decent_size_mess ** 2.;
    odecent_size_mess_tmp := !odecent_size_mess_tmp +. odecent_size_mess ** 2.;
    cent_nb_progressions_tmp := !cent_nb_progressions_tmp +. (float_of_int cent_nb_progressions) ** 2. ;
    decent_nb_progressions_tmp := !decent_nb_progressions_tmp +. (float_of_int decent_nb_progressions) ** 2. ;
    odecent_nb_progressions_tmp := !odecent_nb_progressions_tmp +. (float_of_int odecent_nb_progressions) ** 2.  in
  List.iter summing_square the_list;
  
  let size = List.length the_list in
  
  let 
      dalpha_length_var = variance !dalpha_length_tmp dalpha_length_avg size and
      net_depth_var = variance !net_depth_tmp net_depth_avg size and
      cent_trace_len_var = variance !cent_trace_len_tmp cent_trace_len_avg size  and
      decent_trace_len_var = variance !decent_trace_len_tmp decent_trace_len_avg size and
      odecent_trace_len_var = variance !odecent_trace_len_tmp odecent_trace_len_avg size and
      cent_num_mess_var =  variance !cent_num_mess_tmp cent_num_mess_avg size and
      decent_num_mess_var = variance !decent_num_mess_tmp decent_num_mess_avg size and
      odecent_num_mess_var = variance !odecent_num_mess_tmp odecent_num_mess_avg size and
      cent_size_mess_var = variance !cent_size_mess_tmp cent_size_mess_avg size and
      decent_size_mess_var = variance !decent_size_mess_tmp decent_size_mess_avg size and
      odecent_size_mess_var = variance !odecent_size_mess_tmp odecent_size_mess_avg size and
      cent_nb_progressions_var =  variance !cent_nb_progressions_tmp cent_nb_progressions_avg size and
      decent_nb_progressions_var =  variance !decent_nb_progressions_tmp decent_nb_progressions_avg size and
      odecent_nb_progressions_var = variance !odecent_nb_progressions_tmp odecent_nb_progressions_avg size in

  (dalpha_length_var,
   net_depth_var,
   (float_of_int size),
   cent_trace_len_var,
   decent_trace_len_var,
   odecent_trace_len_var, 
   cent_num_mess_var,
   decent_num_mess_var,
   odecent_num_mess_var,
   cent_size_mess_var,
   decent_size_mess_var,
   odecent_size_mess_var,
   cent_nb_progressions_var,
   decent_nb_progressions_var,
   odecent_nb_progressions_var)

let print_results (dalpha_length, net_depth, number, cent_trace_len_tmp, decent_trace_len_tmp, odecent_trace_len_tmp, cent_num_mess_tmp, decent_num_mess_tmp, odecent_num_mess_tmp, cent_size_mess_tmp, decent_size_mess_tmp, odecent_size_mess_tmp, cent_nb_progressions_tmp, decent_nb_progressions_tmp, odecent_nb_progressions_tmp) =
  let output_string =
    let dalpha_length_string = prepare_display (string_of_float dalpha_length) 7  and
	net_depth_string = prepare_display (string_of_float net_depth) 7  and
	number_string = prepare_display (string_of_float number) 7 and
	cent_trace_len_tmp_string = prepare_display (string_of_float (round cent_trace_len_tmp)) cell_size_medium and
	decent_trace_len_tmp_string = prepare_display (string_of_float (round decent_trace_len_tmp)) cell_size_medium and
	odecent_trace_len_tmp_string = prepare_display (string_of_float (round odecent_trace_len_tmp)) cell_size_medium and
	cent_num_mess_tmp_string = prepare_display (string_of_float (round cent_num_mess_tmp)) cell_size_medium and
	decent_num_mess_tmp_string = prepare_display (string_of_float (round decent_num_mess_tmp)) cell_size_medium and
	odecent_num_mess_tmp_string = prepare_display (string_of_float (round odecent_num_mess_tmp)) cell_size_medium and
	cent_size_mess_tmp_string = prepare_display (string_of_float (round cent_size_mess_tmp)) cell_size_medium and
	decent_size_mess_tmp_string = prepare_display (string_of_float (round decent_size_mess_tmp)) cell_size_medium and
	odecent_size_mess_tmp_string = prepare_display (string_of_float (round odecent_size_mess_tmp)) cell_size_medium and
	cent_nb_progressions_tmp_string = prepare_display (string_of_float (round cent_nb_progressions_tmp)) cell_size_medium and
	decent_nb_progressions_tmp_string = prepare_display (string_of_float (round decent_nb_progressions_tmp)) cell_size_medium and
	odecent_nb_progressions_tmp_string = prepare_display (string_of_float (round odecent_nb_progressions_tmp)) cell_size_medium and
	comma = "|" in
    dalpha_length_string
    ^comma^net_depth_string
    ^comma^number_string
    ^comma^comma
    ^cent_trace_len_tmp_string^comma^cent_num_mess_tmp_string^comma^cent_size_mess_tmp_string^comma^cent_nb_progressions_tmp_string
    ^comma^comma
    ^decent_trace_len_tmp_string^comma^decent_num_mess_tmp_string^comma^decent_size_mess_tmp_string^comma^decent_nb_progressions_tmp_string
    ^comma^comma
    ^odecent_trace_len_tmp_string^comma^odecent_num_mess_tmp_string^comma^odecent_size_mess_tmp_string^comma^odecent_nb_progressions_tmp_string    
    ^comma^comma
  in
  print_endline output_string
    
let nbcomp_depth_test (size_form:int) =

  let res_list = ref [] in
  
  print_endline "       |       |       ||            orchestration              ||                migration              ||              choreography                  ";
  print_endline "|dal.| | depth | numb. ||  |tr|   |   #msg  |  |msg|  |  #prog  ||  |tr|   |   #msg  |  |msg|  |  #prog  ||  |tr|   |   #msg  |  |msg|  |  #prog  || ";
  for i = 1 to !nbtests do
    let one_res = make_one_eval_test !alphabet size_form !sizetrace in
    let one_res = List.filter (fun elt -> let (meaningful, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = elt in meaningful) one_res in
    res_list := !res_list @ one_res;
  done;
  for sdalpha = 1 to List.length !alphabet do
    let res_list_sdalpha = List.filter (fun elt -> let (_, dalpha_length, _, _, _, _, _, _, _, _, _, _, _, _, _) = elt in dalpha_length = sdalpha) !res_list in
    print_separation_line_eval ();
    for depth = 1 to 2 * sdalpha do
      let res_list_sdalpha_depth = List.filter (fun elt -> let (_, _, d, _, _, _, _, _, _, _, _, _, _, _, _) = elt in d = depth) res_list_sdalpha in
      if (res_list_sdalpha_depth <> []) then
	(
	  let res = compute_avg res_list_sdalpha_depth and res_var = compute_var res_list_sdalpha_depth in
	  print_results res;
	  print_results res_var
	)
    done
  done;
  print_separation_line_eval ()


  
let perform_test (size_form:int) =
  
  cent_trace_len := 0; decent_trace_len := 0; odecent_trace_len := 0; cent_num_mess := 0; decent_num_mess := 0; odecent_num_mess := 0; cent_size_mess := 0.; decent_size_mess := 0.; odecent_size_mess := 0.; cent_nb_progressions := 0; decent_nb_progressions := 0; odecent_nb_progressions := 0; max_delay := 0; max_odelay := 0; delay := 0; odelay := 0;

  cent_trace_len_var := 0.; decent_trace_len_var := 0.; odecent_trace_len_var := 0.; cent_num_mess_var := 0.; decent_num_mess_var := 0.; odecent_num_mess_var := 0.; cent_size_mess_var := 0.; decent_size_mess_var := 0.; odecent_size_mess_var := 0.; cent_nb_progressions_var := 0.; decent_nb_progressions_var := 0.; odecent_nb_progressions_var := 0.; delay_var := 0.; odelay_var := 0.;

  cent_num_mess_trin := 0.; decent_num_mess_trin := 0.; odecent_num_mess_trin := 0.; cent_size_mess_trin := 0.; decent_size_mess_trin := 0.; odecent_size_mess_trin := 0.; cent_nb_progressions_trin := 0.; decent_nb_progressions_trin := 0.; odecent_nb_progressions_trin := 0. ;

  
  counter_tests := 0;
  counter_samples := 0;
  while (!counter_samples < !nb_samples && !counter_tests < !nbtests) do
    let (meaningful, cent_trace_len_tmp, decent_trace_len_tmp, odecent_trace_len_tmp, cent_num_mess_tmp, decent_num_mess_tmp, odecent_num_mess_tmp, cent_size_mess_tmp, decent_size_mess_tmp, odecent_size_mess_tmp, cent_nb_progressions_tmp, decent_nb_progressions_tmp, odecent_nb_progressions_tmp)
	= generate_compared_results_efficient !dalphabet size_form !sizetrace !bias in
    (
      counter_tests := !counter_tests + 1;
      if meaningful then
	(
	  counter_samples := !counter_samples + 1;
	  if (!keep_samples) then
	    (
	      let s_size_form = string_of_int (size_form)
	      and s_cent_trace_len = string_of_int (cent_trace_len_tmp)
	      and s_decent_trace_len = string_of_int (decent_trace_len_tmp)
	      and s_odecent_trace_len = string_of_int (odecent_trace_len_tmp)
	      and s_cent_num_mess = string_of_int (cent_num_mess_tmp)
	      and s_decent_num_mess = string_of_int (decent_num_mess_tmp)
	      and s_odecent_num_mess = string_of_int (odecent_num_mess_tmp)
	      and s_cent_size_mess = string_of_float (round_generic cent_size_mess_tmp 2)
	      and s_decent_size_mess = string_of_float (round_generic decent_size_mess_tmp 2)
	      and s_odecent_size_mess = string_of_float (round_generic odecent_size_mess_tmp 2)
	      and s_cent_nb_progressions = string_of_int (cent_nb_progressions_tmp)
	      and s_decent_nb_progressions = string_of_int (decent_nb_progressions_tmp)
	      and s_odecent_nb_progressions = string_of_int (odecent_nb_progressions_tmp)
	      in
	      let space = " " in
	      let one_line = s_size_form^space^s_cent_trace_len^space^s_decent_trace_len^space^s_odecent_trace_len^space^s_cent_num_mess^space^s_decent_num_mess^space^s_odecent_num_mess^space^s_cent_size_mess^space^s_decent_size_mess^space^s_odecent_size_mess^space^s_cent_nb_progressions^space^s_decent_nb_progressions^space^s_odecent_nb_progressions  in
	      write_to_file !file_name one_line
	    );
	  
	  (* Computations for the average *)
	  cent_trace_len := !cent_trace_len + cent_trace_len_tmp;
	  decent_trace_len :=  !decent_trace_len + decent_trace_len_tmp;
	  odecent_trace_len :=  !odecent_trace_len + odecent_trace_len_tmp;
	  cent_num_mess := !cent_num_mess + cent_num_mess_tmp;
	  decent_num_mess := !decent_num_mess + decent_num_mess_tmp;
	  odecent_num_mess := !odecent_num_mess + odecent_num_mess_tmp;
	  delay := !delay + decent_trace_len_tmp - cent_trace_len_tmp;	
	  odelay := !odelay + odecent_trace_len_tmp - cent_trace_len_tmp;
	  cent_size_mess := !cent_size_mess +. cent_size_mess_tmp;
	  decent_size_mess := !decent_size_mess +. decent_size_mess_tmp;
	  odecent_size_mess := !odecent_size_mess +. odecent_size_mess_tmp;
	  cent_nb_progressions := !cent_nb_progressions + cent_nb_progressions_tmp;
	  decent_nb_progressions := !decent_nb_progressions + decent_nb_progressions_tmp;
	  odecent_nb_progressions := !odecent_nb_progressions + odecent_nb_progressions_tmp;
	  if (!max_delay < (decent_trace_len_tmp - cent_trace_len_tmp)) then
	    (  
	      max_delay := decent_trace_len_tmp-cent_trace_len_tmp
	    );
	  if (!max_odelay < (odecent_trace_len_tmp - cent_trace_len_tmp)) then
	    (  
	      max_odelay := odecent_trace_len_tmp - cent_trace_len_tmp
	    );

	  (* Computations for the variance *)
	  cent_trace_len_var := !cent_trace_len_var +. ((float_of_int cent_trace_len_tmp)**2.);
	  decent_trace_len_var := !decent_trace_len_var +. (float_of_int decent_trace_len_tmp)**2.;
	  odecent_trace_len_var := !odecent_trace_len_var +. (float_of_int odecent_trace_len_tmp)**2.;
	  cent_num_mess_var := !cent_num_mess_var +. (float_of_int cent_num_mess_tmp)**2.;
	  decent_num_mess_var := !decent_num_mess_var +. (float_of_int decent_num_mess_tmp)**2.;
	  odecent_num_mess_var := !odecent_num_mess_var +. (float_of_int odecent_num_mess_tmp)**2.;
	  delay_var := !delay_var +. (float_of_int (decent_trace_len_tmp - cent_trace_len_tmp))**2.;	
	  odelay_var := !odelay_var +. (float_of_int (odecent_trace_len_tmp - cent_trace_len_tmp))**2.;
	  cent_size_mess_var := !cent_size_mess_var +. (cent_size_mess_tmp)**2.;
	  decent_size_mess_var := !decent_size_mess_var +. (decent_size_mess_tmp)**2.;
	  odecent_size_mess_var := !odecent_size_mess_var +. (odecent_size_mess_tmp)**2.;
	  cent_nb_progressions_var := !cent_nb_progressions_var +. (float_of_int cent_nb_progressions_tmp)**2.;
	  decent_nb_progressions_var := !decent_nb_progressions_var +. (float_of_int decent_nb_progressions_tmp)**2.;
	  odecent_nb_progressions_var := !odecent_nb_progressions_var +. (float_of_int odecent_nb_progressions_tmp)**2.;

 	  (* Computations for trace insensitive values *)
	  cent_num_mess_trin := !cent_num_mess_trin +. float_of_int cent_num_mess_tmp /. float_of_int cent_trace_len_tmp;
	  decent_num_mess_trin := !decent_num_mess_trin +. float_of_int decent_num_mess_tmp /. float_of_int decent_trace_len_tmp;
	  odecent_num_mess_trin := !odecent_num_mess_trin +. float_of_int odecent_num_mess_tmp /. float_of_int odecent_trace_len_tmp;
	  cent_size_mess_trin := !cent_size_mess_trin +. cent_size_mess_tmp /. float_of_int cent_trace_len_tmp;
	  decent_size_mess_trin := !decent_size_mess_trin +. decent_size_mess_tmp /. float_of_int decent_trace_len_tmp;
	  odecent_size_mess_trin := !odecent_size_mess_trin +. odecent_size_mess_tmp /. float_of_int odecent_trace_len_tmp;
	  cent_nb_progressions_trin := !cent_nb_progressions_trin +. float_of_int cent_nb_progressions_tmp /. float_of_int cent_trace_len_tmp;
	  decent_nb_progressions_trin := !decent_nb_progressions_trin +. float_of_int decent_nb_progressions_tmp /. float_of_int decent_trace_len_tmp;
	  odecent_nb_progressions_trin := !odecent_nb_progressions_trin +. float_of_int odecent_nb_progressions_tmp /. float_of_int odecent_trace_len_tmp;

	)
    )
  done;

  (** average computation **)
  cent_trace_len_avg := round (float_of_int(!cent_trace_len) /. float_of_int(!counter_samples));
  decent_trace_len_avg := round (float_of_int(!decent_trace_len) /. float_of_int(!counter_samples)) ;
  odecent_trace_len_avg := round (float_of_int(!odecent_trace_len) /. float_of_int(!counter_samples));
  cent_num_mess_avg := round (float_of_int(!cent_num_mess) /. float_of_int(!counter_samples));
  decent_num_mess_avg := round (float_of_int(!decent_num_mess) /. float_of_int(!counter_samples));
  odecent_num_mess_avg := round (float_of_int(!odecent_num_mess) /. float_of_int(!counter_samples));
  delay_avg := round (float_of_int(!delay)/. float_of_int(!counter_samples));	
  odelay_avg := round (float_of_int(!odelay) /. float_of_int(!counter_samples));
  cent_size_mess_avg :=  round (!cent_size_mess /. float_of_int(!counter_samples)) ;
  decent_size_mess_avg := round (!decent_size_mess /. float_of_int(!counter_samples)) ;
  odecent_size_mess_avg := round (!odecent_size_mess /. float_of_int(!counter_samples));
  cent_nb_progressions_avg := round (float_of_int(!cent_nb_progressions) /. float_of_int(!counter_samples));
  decent_nb_progressions_avg := round (float_of_int(!decent_nb_progressions) /. float_of_int(!counter_samples)) ;
  odecent_nb_progressions_avg := round (float_of_int(!odecent_nb_progressions) /. float_of_int(!counter_samples));

  (** variance computation **)
  cent_trace_len_var := variance !cent_trace_len_var !cent_trace_len_avg !counter_samples;
  decent_trace_len_var := variance !decent_trace_len_var !decent_trace_len_avg !counter_samples;
  odecent_trace_len_var := variance !odecent_trace_len_var !odecent_trace_len_avg !counter_samples;
  cent_num_mess_var := variance !cent_num_mess_var !cent_num_mess_avg !counter_samples;
  decent_num_mess_var := variance !decent_num_mess_var !decent_num_mess_avg !counter_samples;
  odecent_num_mess_var := variance !odecent_num_mess_var !odecent_num_mess_avg !counter_samples;
  delay_var := variance !delay_var !delay_avg !counter_samples;
  odelay_var := variance !odelay_var !odelay_avg !counter_samples;
  cent_size_mess_var := variance !cent_size_mess_var !cent_size_mess_avg !counter_samples;
  decent_size_mess_var := variance !decent_size_mess_var !decent_size_mess_avg !counter_samples;
  odecent_size_mess_var := variance !odecent_size_mess_var !odecent_size_mess_avg !counter_samples;
  cent_nb_progressions_var := variance !cent_nb_progressions_var !cent_nb_progressions_avg !counter_samples;
  decent_nb_progressions_var := variance !decent_nb_progressions_var !decent_nb_progressions_avg !counter_samples;
  odecent_nb_progressions_var := variance !odecent_nb_progressions_var !odecent_nb_progressions_avg !counter_samples;

  (** average computation for trace independent values **)

  cent_num_mess_trin := round (!cent_num_mess_trin /. float_of_int(!counter_samples));
  decent_num_mess_trin := round (!decent_num_mess_trin /. float_of_int(!counter_samples));
  odecent_num_mess_trin := round (!odecent_num_mess_trin /. float_of_int(!counter_samples));
  cent_size_mess_trin := round (!cent_size_mess_trin /. float_of_int(!counter_samples));
  decent_size_mess_trin := round (!decent_size_mess_trin /. float_of_int(!counter_samples));
  odecent_size_mess_trin := round (!odecent_size_mess_trin /. float_of_int(!counter_samples));
  cent_nb_progressions_trin := round (!cent_nb_progressions_trin /. float_of_int(!counter_samples));
  decent_nb_progressions_trin := round (!decent_nb_progressions_trin /. float_of_int(!counter_samples));
  odecent_nb_progressions_trin := round (!odecent_nb_progressions_trin /. float_of_int(!counter_samples));

  
  let r_counter_samples = string_of_int (!counter_samples)
  and r_sizeform = string_of_int (size_form)
  and r_size_alpha =  if (String.length (stringrep_alphabet (globalAlphabet(!dalphabet))) < 13) then stringrep_alphabet (globalAlphabet(!dalphabet)) else "-"
  and r_size_dalpha = if (String.length (stringrep_dalphabet !dalphabet) < 13) then stringrep_dalphabet !dalphabet else "-"
  and r_cent_trace_len = string_of_float (!cent_trace_len_avg)
  and r_cent_trace_len_var = parenthesize (string_of_float (!cent_trace_len_var))
  and r_cent_num_mess = string_of_float (!cent_num_mess_avg)
  and r_cent_num_mess_var = parenthesize (string_of_float (!cent_num_mess_var))
  and r_cent_num_mess_trin = parenthesize2 (string_of_float (!cent_num_mess_trin))
  and r_cent_size_mess = string_of_float (!cent_size_mess_avg)
  and r_cent_size_mess_var = parenthesize (string_of_float (!cent_size_mess_var))
  and r_cent_size_mess_trin = parenthesize2 (string_of_float (!cent_size_mess_trin))
  and r_cent_nb_progressions = string_of_float (!cent_nb_progressions_avg)
  and r_cent_nb_progressions_var = parenthesize (string_of_float (!cent_nb_progressions_var))
  and r_cent_nb_progressions_trin = parenthesize2 (string_of_float (!cent_nb_progressions_trin))
  and r_decent_trace_len = string_of_float(!decent_trace_len_avg)
  and r_decent_trace_len_var = parenthesize (string_of_float(!decent_trace_len_var))
  and r_decent_num_mess = string_of_float(!decent_num_mess_avg)
  and r_decent_num_mess_var = parenthesize (string_of_float(!decent_num_mess_var))
  and r_decent_num_mess_trin = parenthesize2 (string_of_float(!decent_num_mess_trin))
  and r_decent_size_mess = string_of_float(!decent_size_mess_avg)
  and r_decent_size_mess_var = parenthesize (string_of_float(!decent_size_mess_var))
  and r_decent_size_mess_trin = parenthesize2 (string_of_float(!decent_size_mess_trin))
  and r_decent_nb_progressions = string_of_float (!decent_nb_progressions_avg)
  and r_decent_nb_progressions_var = parenthesize (string_of_float (!decent_nb_progressions_var))
  and r_decent_nb_progressions_trin = parenthesize2 (string_of_float (!decent_nb_progressions_trin))
  and r_odecent_trace_len = string_of_float (!odecent_trace_len_avg)
  and r_odecent_trace_len_var = parenthesize (string_of_float (!odecent_trace_len_var))
  and r_odecent_num_mess = string_of_float(!odecent_num_mess_avg)
  and r_odecent_num_mess_var = parenthesize (string_of_float(!odecent_num_mess_var))
  and r_odecent_num_mess_trin = parenthesize2 (string_of_float(!odecent_num_mess_trin))
  and r_odecent_size_mess = string_of_float(!odecent_size_mess_avg)
  and r_odecent_size_mess_var = parenthesize (string_of_float(!odecent_size_mess_var))
  and r_odecent_size_mess_trin = parenthesize2 (string_of_float(!odecent_size_mess_trin))
  and r_odecent_nb_progressions = string_of_float (!odecent_nb_progressions_avg)
  and r_odecent_nb_progressions_var = parenthesize (string_of_float (!odecent_nb_progressions_var))
  and r_odecent_nb_progressions_trin = parenthesize2 (string_of_float (!odecent_nb_progressions_trin))
  and r_diff_trace_len = string_of_float(round (float_of_int(!decent_trace_len) /.float_of_int(!cent_trace_len)))
  and r_diff_num_mess = string_of_float(round (float_of_int(!decent_num_mess)/. float_of_int(!cent_num_mess)))
  and r_diff_size_mess = string_of_float(round (!decent_size_mess/. !cent_size_mess ))
  and r_diff_nb_progressions = string_of_float(round (float_of_int !decent_nb_progressions /. float_of_int !cent_nb_progressions ))
  and r_odiff_trace_len = string_of_float(round (float_of_int(!odecent_trace_len) /.float_of_int(!cent_trace_len) ))
  and r_odiff_num_mess = string_of_float(round (float_of_int(!odecent_num_mess)/. float_of_int(!cent_num_mess) ))
  and r_odiff_size_mess = string_of_float(round (!odecent_size_mess /. !cent_size_mess))
  and r_odiff_nb_progressions = string_of_float(round (float_of_int !odecent_nb_progressions /. float_of_int !cent_nb_progressions ))
  and r_delay = string_of_float(!delay_avg)
  and r_delay_var = parenthesize (string_of_float(!delay_var))
  and r_odelay = string_of_float(!odelay_avg)
  and r_odelay_var = parenthesize (string_of_float(!odelay_var))
  and r_max = string_of_int(!max_delay)
  and r_omax = string_of_int(!max_odelay)

  in
  
  let r_counter_samples = prepare_display r_counter_samples 4
  and r_sizeform = prepare_display r_sizeform 6
  and r_size_alpha = prepare_display r_size_alpha cell_size_large
  and r_size_dalpha = prepare_display r_size_dalpha cell_size_large
  and r_cent_trace_len = prepare_display r_cent_trace_len cell_size_medium
  and r_cent_trace_len_var = prepare_display r_cent_trace_len_var cell_size_medium
  and r_cent_num_mess = prepare_display r_cent_num_mess cell_size_medium
  and r_cent_num_mess_var = prepare_display r_cent_num_mess_var cell_size_medium
  and r_cent_num_mess_trin = prepare_display r_cent_num_mess_trin cell_size_medium
  and r_cent_size_mess = prepare_display r_cent_size_mess cell_size_medium
  and r_cent_size_mess_var = prepare_display r_cent_size_mess_var cell_size_medium
  and r_cent_size_mess_trin = prepare_display r_cent_size_mess_trin cell_size_medium
  and r_cent_nb_progressions = prepare_display r_cent_nb_progressions cell_size_medium
  and r_cent_nb_progressions_var = prepare_display r_cent_nb_progressions_var cell_size_medium
  and r_cent_nb_progressions_trin = prepare_display r_cent_nb_progressions_trin cell_size_medium
  and r_decent_trace_len = prepare_display r_decent_trace_len cell_size_medium
  and r_decent_trace_len_var = prepare_display r_decent_trace_len_var cell_size_medium
  and r_decent_num_mess = prepare_display r_decent_num_mess cell_size_medium
  and r_decent_num_mess_var = prepare_display r_decent_num_mess_var cell_size_medium
  and r_decent_num_mess_trin = prepare_display r_decent_num_mess_trin cell_size_medium
  and r_decent_size_mess = prepare_display r_decent_size_mess cell_size_medium
  and r_decent_size_mess_var = prepare_display r_decent_size_mess_var cell_size_medium
  and r_decent_size_mess_trin = prepare_display r_decent_size_mess_trin cell_size_medium
  and r_decent_nb_progressions = prepare_display r_decent_nb_progressions cell_size_medium
  and r_decent_nb_progressions_var = prepare_display r_decent_nb_progressions_var cell_size_medium
  and r_decent_nb_progressions_trin = prepare_display r_decent_nb_progressions_trin cell_size_medium
  and r_odecent_trace_len = prepare_display r_odecent_trace_len cell_size_medium
  and r_odecent_trace_len_var = prepare_display r_odecent_trace_len_var cell_size_medium
  and r_odecent_num_mess = prepare_display r_odecent_num_mess cell_size_medium
  and r_odecent_num_mess_var = prepare_display r_odecent_num_mess_var cell_size_medium
  and r_odecent_num_mess_trin = prepare_display r_odecent_num_mess_trin cell_size_medium
  and r_odecent_size_mess = prepare_display r_odecent_size_mess cell_size_medium
  and r_odecent_size_mess_var = prepare_display r_odecent_size_mess_var cell_size_medium
  and r_odecent_size_mess_trin = prepare_display r_odecent_size_mess_trin cell_size_medium
  and r_odecent_nb_progressions = prepare_display r_odecent_nb_progressions cell_size_medium
  and r_odecent_nb_progressions_var = prepare_display r_odecent_nb_progressions_var cell_size_medium
  and r_odecent_nb_progressions_trin = prepare_display r_odecent_nb_progressions_trin cell_size_medium
  and r_diff_trace_len = prepare_display r_diff_trace_len cell_size_medium
  and r_diff_num_mess = prepare_display r_diff_num_mess cell_size_medium
  and r_diff_size_mess = prepare_display r_diff_size_mess cell_size_medium
  and r_diff_nb_progressions = prepare_display r_diff_nb_progressions cell_size_medium
  and r_odiff_trace_len = prepare_display r_odiff_trace_len cell_size_medium
  and r_odiff_num_mess = prepare_display r_odiff_num_mess cell_size_medium
  and r_odiff_size_mess = prepare_display r_odiff_size_mess cell_size_medium
  and r_odiff_nb_progressions = prepare_display r_odiff_nb_progressions cell_size_medium
  and r_delay = prepare_display r_delay cell_size_medium
  and r_delay_var = prepare_display r_delay_var cell_size_medium
  and r_odelay = prepare_display r_odelay cell_size_medium
  and r_odelay_var = prepare_display r_odelay_var cell_size_medium
  and r_max = prepare_display r_max cell_size_small
  and r_omax = prepare_display r_omax cell_size_small
    
  in
  if (!print_full_stats) then
    ( 
      print_separator0 ();
      print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_size_mess^"|"^r_cent_nb_progressions^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"|"^r_decent_size_mess^"|"^r_decent_nb_progressions^"||"^r_odecent_trace_len^"|"^r_odecent_num_mess^"|"^r_odecent_size_mess^"|"^r_odecent_nb_progressions^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess^"|"^r_diff_nb_progressions^"||"^r_odiff_trace_len^"|"^r_odiff_num_mess^"|"^r_odiff_size_mess^"|"^r_odiff_nb_progressions^"||"^r_delay^"|"^r_max^"||"^r_odelay^"|"^r_omax);
      
      print_endline ("("^r_counter_samples^")|            |            ||"^r_cent_trace_len_var^"|"^r_cent_num_mess_var^"|"^r_cent_size_mess_var^"|"^r_cent_nb_progressions_var^"||"^r_decent_trace_len_var^"|"^r_decent_num_mess_var^"|"^r_decent_size_mess_var^"|"^r_decent_nb_progressions_var^"||"^r_odecent_trace_len_var^"|"^r_odecent_num_mess_var^"|"^r_odecent_size_mess_var^"|"^r_odecent_nb_progressions_var^"||         |         |         |         ||         |         |         |         ||"^r_delay_var^"|       ||"^r_odelay_var^"|       ");

      print_endline ("      |            |            ||         |"^r_cent_num_mess_trin^"|"^r_cent_size_mess_trin^"|"^r_cent_nb_progressions_trin^"||         |"^r_decent_num_mess_trin^"|"^r_decent_size_mess_trin^"|"^r_decent_nb_progressions_trin^"||         |"^r_odecent_num_mess_trin^"|"^r_odecent_size_mess_trin^"|"^r_odecent_nb_progressions_trin^"||         |         |         |         ||         |         |         |         ||         |       ||         |       ")
    );
  if (!print_delay_stats) then
    ( 
      print_endline("-------------------------------------------------||--------------");
      print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_delay^"|"^r_max^"||"^r_odelay^"|"^r_omax)
    );
  if (!print_trace_and_mess_stats) then
    ( 
      print_endline("-------------------------------------------------||---------------------||---------------------||----------------------");
      print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_decent_size_mess^"||"^r_odecent_trace_len^"|"^r_odecent_num_mess^"|"^r_odecent_size_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess^"||"^r_odiff_trace_len^"|"^r_odiff_num_mess^"|"^r_odiff_size_mess)
    )

let perform_test_patterns (kind:string) =

  cent_trace_len := 0; decent_trace_len := 0; odecent_trace_len := 0; cent_num_mess := 0; decent_num_mess := 0;   odecent_num_mess := 0; cent_size_mess := 0.; decent_size_mess := 0.; odecent_size_mess := 0.; max_delay := 0; max_odelay := 0; cent_nb_progressions := 0; decent_nb_progressions := 0; odecent_nb_progressions := 0; delay := 0; odelay := 0;

  cent_trace_len_var := 0.; decent_trace_len_var := 0.; odecent_trace_len_var := 0.; cent_num_mess_var := 0.; decent_num_mess_var := 0.; odecent_num_mess_var := 0.; cent_size_mess_var := 0.; decent_size_mess_var := 0.; odecent_size_mess_var := 0.; cent_nb_progressions_var := 0.; decent_nb_progressions_var := 0.; odecent_nb_progressions_var := 0.; delay_var := 0.; odelay_var := 0.;

  cent_num_mess_trin := 0.; decent_num_mess_trin := 0.; odecent_num_mess_trin := 0.; cent_size_mess_trin := 0.; decent_size_mess_trin := 0.; odecent_size_mess_trin := 0.; cent_nb_progressions_trin := 0.; decent_nb_progressions_trin := 0.; odecent_nb_progressions_trin := 0. ;
  
  counter_samples:= 0;
  counter_tests := 0;
  while (!counter_tests < !nbtests && !counter_samples < !nb_samples)  do
    let (meaningful, cent_trace_len_tmp,decent_trace_len_tmp,odecent_trace_len_tmp,cent_num_mess_tmp,decent_num_mess_tmp,odecent_num_mess_tmp,cent_size_mess_tmp,decent_size_mess_tmp, odecent_size_mess_tmp, cent_nb_progressions_tmp, decent_nb_progressions_tmp, odecent_nb_progressions_tmp)
	= generate_compared_results_efficient_patterns !dalphabet kind !sizetrace in
    (
      counter_tests := !counter_tests + 1;
      if meaningful then
	(
	  counter_samples := !counter_samples + 1;
	  if (!keep_samples) then
	    (
	      let s_cent_trace_len = string_of_int (cent_trace_len_tmp)
	      and s_decent_trace_len = string_of_int (decent_trace_len_tmp)
	      and s_odecent_trace_len = string_of_int (odecent_trace_len_tmp)
	      and s_cent_num_mess = string_of_int (cent_num_mess_tmp)
	      and s_decent_num_mess = string_of_int (decent_num_mess_tmp)
	      and s_odecent_num_mess = string_of_int (odecent_num_mess_tmp)
	      and s_cent_size_mess = string_of_float (round_generic cent_size_mess_tmp 2)
	      and s_decent_size_mess = string_of_float (round_generic decent_size_mess_tmp 2)
	      and s_odecent_size_mess = string_of_float (round_generic odecent_size_mess_tmp 2)
	      and s_cent_nb_progressions = string_of_int (cent_nb_progressions_tmp)
	      and s_decent_nb_progressions = string_of_int (decent_nb_progressions_tmp)
	      and s_odecent_nb_progressions = string_of_int (odecent_nb_progressions_tmp)
	      in
	      let space = " " in
	      let one_line = kind^space^s_cent_trace_len^space^s_decent_trace_len^space^s_odecent_trace_len^space^s_cent_num_mess^space^s_decent_num_mess^space^s_odecent_num_mess^space^s_cent_size_mess^space^s_decent_size_mess^space^s_odecent_size_mess^space^s_cent_nb_progressions^space^s_decent_nb_progressions^space^s_odecent_nb_progressions  in
	      write_to_file !file_name one_line
	    );
	  
	  cent_trace_len := !cent_trace_len+cent_trace_len_tmp;
	  decent_trace_len :=  !decent_trace_len + decent_trace_len_tmp;
	  odecent_trace_len :=  !odecent_trace_len + odecent_trace_len_tmp;
	  cent_num_mess := !cent_num_mess + cent_num_mess_tmp;
	  decent_num_mess := !decent_num_mess + decent_num_mess_tmp;
	  odecent_num_mess := !odecent_num_mess + odecent_num_mess_tmp;
	  delay := !delay + decent_trace_len_tmp - cent_trace_len_tmp;	
	  odelay := !odelay + odecent_trace_len_tmp - cent_trace_len_tmp;
	  cent_size_mess := !cent_size_mess +. cent_size_mess_tmp;
	  decent_size_mess := !decent_size_mess +. decent_size_mess_tmp;
	  odecent_size_mess := !odecent_size_mess +. odecent_size_mess_tmp;
	  cent_nb_progressions := !cent_nb_progressions + cent_nb_progressions_tmp;
	  decent_nb_progressions := !decent_nb_progressions + decent_nb_progressions_tmp;
	  odecent_nb_progressions := !odecent_nb_progressions + odecent_nb_progressions_tmp;
	  if (!max_delay < (decent_trace_len_tmp - cent_trace_len_tmp)) then
	    (  
	      max_delay := decent_trace_len_tmp-cent_trace_len_tmp
	    );
	  if (!max_odelay < (odecent_trace_len_tmp - cent_trace_len_tmp)) then
	    (  
	      max_odelay := odecent_trace_len_tmp - cent_trace_len_tmp
	    );

	  (* Computations for the variance *)
	  cent_trace_len_var := !cent_trace_len_var +. ((float_of_int cent_trace_len_tmp)**2.);
	  decent_trace_len_var := !decent_trace_len_var +. (float_of_int decent_trace_len_tmp)**2.;
	  odecent_trace_len_var := !odecent_trace_len_var +. (float_of_int odecent_trace_len_tmp)**2.;
	  cent_num_mess_var := !cent_num_mess_var +. (float_of_int cent_num_mess_tmp)**2.;
	  decent_num_mess_var := !decent_num_mess_var +. (float_of_int decent_num_mess_tmp)**2.;
	  odecent_num_mess_var := !odecent_num_mess_var +. (float_of_int odecent_num_mess_tmp)**2.;
	  delay_var := !delay_var +. (float_of_int (decent_trace_len_tmp - cent_trace_len_tmp))**2.;	
	  odelay_var := !odelay_var +. (float_of_int (odecent_trace_len_tmp - cent_trace_len_tmp))**2.;
	  cent_size_mess_var := !cent_size_mess_var +. (cent_size_mess_tmp)**2.;
	  decent_size_mess_var := !decent_size_mess_var +. (decent_size_mess_tmp)**2.;
	  odecent_size_mess_var := !odecent_size_mess_var +. (odecent_size_mess_tmp)**2.;
	  cent_nb_progressions_var := !cent_nb_progressions_var +. (float_of_int cent_nb_progressions_tmp)**2.;
	  decent_nb_progressions_var := !decent_nb_progressions_var +. (float_of_int decent_nb_progressions_tmp)**2.;
	  odecent_nb_progressions_var := !odecent_nb_progressions_var +. (float_of_int odecent_nb_progressions_tmp)**2.;


	  (* Computations for trace insensitive values *)
	  cent_num_mess_trin := !cent_num_mess_trin +. float_of_int cent_num_mess_tmp /. float_of_int cent_trace_len_tmp;
	  decent_num_mess_trin := !decent_num_mess_trin +. float_of_int decent_num_mess_tmp /. float_of_int decent_trace_len_tmp;
	  odecent_num_mess_trin := !odecent_num_mess_trin +. float_of_int odecent_num_mess_tmp /. float_of_int odecent_trace_len_tmp;
	  cent_size_mess_trin := !cent_size_mess_trin +. cent_size_mess_tmp /. float_of_int cent_trace_len_tmp;
	  decent_size_mess_trin := !decent_size_mess_trin +. decent_size_mess_tmp /. float_of_int decent_trace_len_tmp;
	  odecent_size_mess_trin := !odecent_size_mess_trin +. odecent_size_mess_tmp /. float_of_int odecent_trace_len_tmp;
	  cent_nb_progressions_trin := !cent_nb_progressions_trin +. float_of_int cent_nb_progressions_tmp /. float_of_int cent_trace_len_tmp;
	  decent_nb_progressions_trin := !decent_nb_progressions_trin +. float_of_int decent_nb_progressions_tmp /. float_of_int decent_trace_len_tmp;
	  odecent_nb_progressions_trin := !odecent_nb_progressions_trin +. float_of_int odecent_nb_progressions_tmp /. float_of_int odecent_trace_len_tmp;
	)
    )
  done;

  (** average computation **)
  cent_trace_len_avg := round (float_of_int(!cent_trace_len) /. float_of_int(!counter_samples));
  decent_trace_len_avg := round (float_of_int(!decent_trace_len) /. float_of_int(!counter_samples)) ;
  odecent_trace_len_avg := round (float_of_int(!odecent_trace_len) /. float_of_int(!counter_samples));
  cent_num_mess_avg := round (float_of_int(!cent_num_mess) /. float_of_int(!counter_samples));
  decent_num_mess_avg := round (float_of_int(!decent_num_mess) /. float_of_int(!counter_samples));
  odecent_num_mess_avg := round (float_of_int(!odecent_num_mess) /. float_of_int(!counter_samples));
  delay_avg := round (float_of_int(!delay) /. float_of_int(!counter_samples));	
  odelay_avg := round (float_of_int(!odelay) /. float_of_int(!counter_samples));
  cent_size_mess_avg :=  round (!cent_size_mess /. float_of_int(!counter_samples)) ;
  decent_size_mess_avg := round (!decent_size_mess /. float_of_int(!counter_samples)) ;
  odecent_size_mess_avg := round (!odecent_size_mess /. float_of_int(!counter_samples));
  cent_nb_progressions_avg := round (float_of_int(!cent_nb_progressions) /. float_of_int(!counter_samples));
  decent_nb_progressions_avg := round (float_of_int(!decent_nb_progressions) /. float_of_int(!counter_samples)) ;
  odecent_nb_progressions_avg := round (float_of_int(!odecent_nb_progressions) /. float_of_int(!counter_samples));

  (** variance computation **)
  cent_trace_len_var := variance !cent_trace_len_var !cent_trace_len_avg !counter_samples;
  decent_trace_len_var := variance !decent_trace_len_var !decent_trace_len_avg !counter_samples;
  odecent_trace_len_var :=  variance !odecent_trace_len_var !odecent_trace_len_avg !counter_samples;
  cent_num_mess_var := variance !cent_num_mess_var !cent_num_mess_avg !counter_samples;
  decent_num_mess_var := variance !decent_num_mess_var !decent_num_mess_avg !counter_samples;
  odecent_num_mess_var := variance !odecent_num_mess_var !odecent_num_mess_avg !counter_samples;
  delay_var := variance !delay_var !delay_avg !counter_samples;
  odelay_var := variance !odelay_var !odelay_avg !counter_samples;
  cent_size_mess_var := variance !cent_size_mess_var !cent_size_mess_avg !counter_samples;
  decent_size_mess_var := variance !decent_size_mess_var !decent_size_mess_avg !counter_samples;
  odecent_size_mess_var := variance !odecent_size_mess_var !odecent_size_mess_avg !counter_samples;
  cent_nb_progressions_var := variance !cent_nb_progressions_var !cent_nb_progressions_avg !counter_samples;
  decent_nb_progressions_var := variance !decent_nb_progressions_var !decent_nb_progressions_avg !counter_samples;
  odecent_nb_progressions_var := variance !odecent_nb_progressions_var !odecent_nb_progressions_avg !counter_samples;

  (** average computation for trace independent values **)

  cent_num_mess_trin := round (!cent_num_mess_trin /. float_of_int(!counter_samples));
  decent_num_mess_trin := round (!decent_num_mess_trin /. float_of_int(!counter_samples));
  odecent_num_mess_trin := round (!odecent_num_mess_trin /. float_of_int(!counter_samples));
  cent_size_mess_trin := round (!cent_size_mess_trin /. float_of_int(!counter_samples));
  decent_size_mess_trin := round (!decent_size_mess_trin /. float_of_int(!counter_samples));
  odecent_size_mess_trin := round (!odecent_size_mess_trin /. float_of_int(!counter_samples));
  cent_nb_progressions_trin := round (!cent_nb_progressions_trin /. float_of_int(!counter_samples));
  decent_nb_progressions_trin := round (!decent_nb_progressions_trin /. float_of_int(!counter_samples));
  odecent_nb_progressions_trin := round (!odecent_nb_progressions_trin /. float_of_int(!counter_samples));

  
  let r_form = kind
  and r_counter_samples = string_of_int (!counter_samples)
  and r_size_alpha =  if (String.length (stringrep_alphabet (globalAlphabet(!dalphabet))) < 9) then stringrep_alphabet (globalAlphabet(!dalphabet)) else "-"
  and r_size_dalpha = if (String.length (stringrep_dalphabet !dalphabet) < 10) then stringrep_dalphabet !dalphabet else "-"
  and r_cent_trace_len = string_of_float (!cent_trace_len_avg)
  and r_cent_trace_len_var = parenthesize (string_of_float (!cent_trace_len_var))
  and r_cent_num_mess = string_of_float (!cent_num_mess_avg)
  and r_cent_num_mess_var = parenthesize (string_of_float (!cent_num_mess_var))
  and r_cent_num_mess_trin = parenthesize2 (string_of_float (!cent_num_mess_trin))
  and r_cent_size_mess = string_of_float (!cent_size_mess_avg)
  and r_cent_size_mess_var = parenthesize (string_of_float (!cent_size_mess_var))
  and r_cent_size_mess_trin = parenthesize2 (string_of_float (!cent_size_mess_trin))
  and r_cent_nb_progressions = string_of_float (!cent_nb_progressions_avg)
  and r_cent_nb_progressions_var = parenthesize (string_of_float (!cent_nb_progressions_var))
  and r_cent_nb_progressions_trin = parenthesize2 (string_of_float (!cent_nb_progressions_trin))
  and r_decent_trace_len = string_of_float(!decent_trace_len_avg)
  and r_decent_trace_len_var = parenthesize (string_of_float(!decent_trace_len_var))
  and r_decent_num_mess = string_of_float(!decent_num_mess_avg)
  and r_decent_num_mess_var = parenthesize (string_of_float(!decent_num_mess_var))
  and r_decent_num_mess_trin = parenthesize2 (string_of_float(!decent_num_mess_trin))
  and r_decent_size_mess = string_of_float(!decent_size_mess_avg)
  and r_decent_size_mess_var = parenthesize (string_of_float(!decent_size_mess_var))
  and r_decent_size_mess_trin = parenthesize2 (string_of_float(!decent_size_mess_trin))
  and r_decent_nb_progressions = string_of_float (!decent_nb_progressions_avg)
  and r_decent_nb_progressions_var = parenthesize (string_of_float (!decent_nb_progressions_var))
  and r_decent_nb_progressions_trin = parenthesize2 (string_of_float (!decent_nb_progressions_trin))
  and r_odecent_trace_len = string_of_float (!odecent_trace_len_avg)
  and r_odecent_trace_len_var = parenthesize (string_of_float (!odecent_trace_len_var))
  and r_odecent_num_mess = string_of_float(!odecent_num_mess_avg)
  and r_odecent_num_mess_var = parenthesize (string_of_float(!odecent_num_mess_var))
  and r_odecent_num_mess_trin = parenthesize2 (string_of_float(!odecent_num_mess_trin))
  and r_odecent_size_mess = string_of_float(!odecent_size_mess_avg)
  and r_odecent_size_mess_var = parenthesize (string_of_float(!odecent_size_mess_var))
  and r_odecent_size_mess_trin = parenthesize2 (string_of_float(!odecent_size_mess_trin))
  and r_odecent_nb_progressions = string_of_float (!odecent_nb_progressions_avg)
  and r_odecent_nb_progressions_var = parenthesize (string_of_float (!odecent_nb_progressions_var))
  and r_odecent_nb_progressions_trin = parenthesize2 (string_of_float (!odecent_nb_progressions_trin))
  and r_diff_trace_len = string_of_float(round (float_of_int(!decent_trace_len) /.float_of_int(!cent_trace_len)))
  and r_diff_num_mess = string_of_float(round (float_of_int(!decent_num_mess)/. float_of_int(!cent_num_mess)))
  and r_diff_size_mess = string_of_float(round (!decent_size_mess/. !cent_size_mess ))
  and r_diff_nb_progressions = string_of_float(round (float_of_int !decent_nb_progressions /. float_of_int !cent_nb_progressions ))
  and r_odiff_trace_len = string_of_float(round (float_of_int(!odecent_trace_len) /.float_of_int(!cent_trace_len) ))
  and r_odiff_num_mess = string_of_float(round (float_of_int(!odecent_num_mess)/. float_of_int(!cent_num_mess) ))
  and r_odiff_size_mess = string_of_float(round (!odecent_size_mess /. !cent_size_mess))
  and r_odiff_nb_progressions = string_of_float(round (float_of_int !odecent_nb_progressions /. float_of_int !cent_nb_progressions ))
  and r_delay = string_of_float(!delay_avg)
  and r_delay_var = parenthesize (string_of_float(!delay_var))
  and r_odelay = string_of_float(!odelay_avg)
  and r_odelay_var = parenthesize (string_of_float(!odelay_var))
  and r_max = string_of_int(!max_delay)
  and r_omax = string_of_int(!max_odelay)
    
  in
  
  let r_counter_samples = prepare_display r_counter_samples 4
  and r_form = prepare_display r_form 6
  and r_size_alpha = prepare_display r_size_alpha cell_size_large
  and r_size_dalpha = prepare_display r_size_dalpha cell_size_large
  and r_cent_trace_len = prepare_display r_cent_trace_len cell_size_medium
  and r_cent_trace_len_var = prepare_display r_cent_trace_len_var cell_size_medium
  and r_cent_num_mess = prepare_display r_cent_num_mess cell_size_medium
  and r_cent_num_mess_var = prepare_display r_cent_num_mess_var cell_size_medium
  and r_cent_num_mess_trin = prepare_display r_cent_num_mess_trin cell_size_medium
  and r_cent_size_mess = prepare_display r_cent_size_mess cell_size_medium
  and r_cent_size_mess_var = prepare_display r_cent_size_mess_var cell_size_medium
  and r_cent_size_mess_trin = prepare_display r_cent_size_mess_trin cell_size_medium
  and r_cent_nb_progressions = prepare_display r_cent_nb_progressions cell_size_medium
  and r_cent_nb_progressions_var = prepare_display r_cent_nb_progressions_var cell_size_medium
  and r_cent_nb_progressions_trin = prepare_display r_cent_nb_progressions_trin cell_size_medium
  and r_decent_trace_len = prepare_display r_decent_trace_len cell_size_medium
  and r_decent_trace_len_var = prepare_display r_decent_trace_len_var cell_size_medium
  and r_decent_num_mess = prepare_display r_decent_num_mess cell_size_medium
  and r_decent_num_mess_var = prepare_display r_decent_num_mess_var cell_size_medium
  and r_decent_num_mess_trin = prepare_display r_decent_num_mess_trin cell_size_medium
  and r_decent_size_mess = prepare_display r_decent_size_mess cell_size_medium
  and r_decent_size_mess_var = prepare_display r_decent_size_mess_var cell_size_medium
  and r_decent_size_mess_trin = prepare_display r_decent_size_mess_trin cell_size_medium
  and r_decent_nb_progressions = prepare_display r_decent_nb_progressions cell_size_medium
  and r_decent_nb_progressions_var = prepare_display r_decent_nb_progressions_var cell_size_medium
  and r_decent_nb_progressions_trin = prepare_display r_decent_nb_progressions_trin cell_size_medium
  and r_odecent_trace_len = prepare_display r_odecent_trace_len cell_size_medium
  and r_odecent_trace_len_var = prepare_display r_odecent_trace_len_var cell_size_medium
  and r_odecent_num_mess = prepare_display r_odecent_num_mess cell_size_medium
  and r_odecent_num_mess_var = prepare_display r_odecent_num_mess_var cell_size_medium
  and r_odecent_num_mess_trin = prepare_display r_odecent_num_mess_trin cell_size_medium
  and r_odecent_size_mess = prepare_display r_odecent_size_mess cell_size_medium
  and r_odecent_size_mess_var = prepare_display r_odecent_size_mess_var cell_size_medium
  and r_odecent_size_mess_trin = prepare_display r_odecent_size_mess_trin cell_size_medium
  and r_odecent_nb_progressions = prepare_display r_odecent_nb_progressions cell_size_medium
  and r_odecent_nb_progressions_var = prepare_display r_odecent_nb_progressions_var cell_size_medium
  and r_odecent_nb_progressions_trin = prepare_display r_odecent_nb_progressions_trin cell_size_medium
  and r_diff_trace_len = prepare_display r_diff_trace_len cell_size_medium
  and r_diff_num_mess = prepare_display r_diff_num_mess cell_size_medium
  and r_diff_size_mess = prepare_display r_diff_size_mess cell_size_medium
  and r_diff_nb_progressions = prepare_display r_diff_nb_progressions cell_size_medium
  and r_odiff_trace_len = prepare_display r_odiff_trace_len cell_size_medium
  and r_odiff_num_mess = prepare_display r_odiff_num_mess cell_size_medium
  and r_odiff_size_mess = prepare_display r_odiff_size_mess cell_size_medium
  and r_odiff_nb_progressions = prepare_display r_odiff_nb_progressions cell_size_medium
  and r_delay = prepare_display r_delay cell_size_medium
  and r_delay_var = prepare_display r_delay_var cell_size_medium
  and r_odelay = prepare_display r_odelay cell_size_medium
  and r_odelay_var = prepare_display r_odelay_var cell_size_medium
  and r_max = prepare_display r_max cell_size_small
  and r_omax = prepare_display r_omax cell_size_small
    
  in
  ( 
    print_separator0 ();
    print_endline (r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_size_mess^"|"^r_cent_nb_progressions^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"|"^r_decent_size_mess^"|"^r_decent_nb_progressions^"||"^r_odecent_trace_len^"|"^r_odecent_num_mess^"|"^r_odecent_size_mess^"|"^r_odecent_nb_progressions^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess^"|"^r_diff_nb_progressions^"||"^r_odiff_trace_len^"|"^r_odiff_num_mess^"|"^r_odiff_size_mess^"|"^r_odiff_nb_progressions^"||"^r_delay^"|"^r_max^"||"^r_odelay^"|"^r_omax)
  );
  print_endline ("("^r_counter_samples^")|            |            ||"^r_cent_trace_len_var^"|"^r_cent_num_mess_var^"|"^r_cent_size_mess_var^"|"^r_cent_nb_progressions_var^"||"^r_decent_trace_len_var^"|"^r_decent_num_mess_var^"|"^r_decent_size_mess_var^"|"^r_decent_nb_progressions_var^"||"^r_odecent_trace_len_var^"|"^r_odecent_num_mess_var^"|"^r_odecent_size_mess_var^"|"^r_odecent_nb_progressions_var^"||         |         |         |         ||         |         |         |         ||"^r_delay_var^"|       ||"^r_odelay_var^"|       ");

  print_endline ("      |            |            ||         |"^r_cent_num_mess_trin^"|"^r_cent_size_mess_trin^"|"^r_cent_nb_progressions_trin^"||         |"^r_decent_num_mess_trin^"|"^r_decent_size_mess_trin^"|"^r_decent_nb_progressions_trin^"||         |"^r_odecent_num_mess_trin^"|"^r_odecent_size_mess_trin^"|"^r_odecent_nb_progressions_trin^"||         |         |         |         ||         |         |         |         ||         |       ||         |       ");
  
  
  
  if (!print_delay_stats) then
    ( 
      print_endline("-------------------------------------------------||--------------");
      print_endline (r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_delay^"|"^r_max^"||"^r_odelay^"|"^r_omax)
    );
  if (!print_trace_and_mess_stats) then
    ( 
      print_endline("-------------------------------------------------||---------------------||---------------------||----------------------");
      print_endline (r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_decent_size_mess^"||"^r_odecent_trace_len^"|"^r_odecent_num_mess^"|"^r_odecent_size_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess^"||"^r_odiff_trace_len^"|"^r_odiff_num_mess^"|"^r_odiff_size_mess)
    )

(**
let do_pattern_test_group_results (kind:string) (dalphabets_to_consider: d_alphabet list) (alpha:alphabet) =
  !cent_trace_len := 0;
  !decent_trace_len := 0;
  !cent_num_mess := 0;
  !decent_num_mess := 0;
  for i=0 to (List.length dalphabets_to_consider)-1 do
    dalphabet:= List.nth dalphabets_to_consider i;
    for i=1 to !nbtests do
      let (form,cent_trace_len_tmp,decent_trace_len_tmp,cent_num_mess_tmp,decent_num_mess_tmp,cent_size_mess_tmp,decent_size_mess_tmp) = 
	generate_compared_results_efficient_patterns !dalphabet kind !sizetrace in (
	  !cent_trace_len := !cent_trace_len+cent_trace_len_tmp;
	  !decent_trace_len := !decent_trace_len + decent_trace_len_tmp;
	  !cent_num_mess := !cent_num_mess + cent_num_mess_tmp;
	  !decent_num_mess := !decent_num_mess + decent_num_mess_tmp;
	  !cent_size_mess := !cent_size_mess + cent_size_mess_tmp;
	  !decent_size_mess := !decent_size_mess + decent_size_mess_tmp;
	  formula := form;
	  !delay := !delay + decent_trace_len_tmp - cent_trace_len_tmp;	
	  if (!max_delay < (decent_trace_len_tmp-cent_trace_len_tmp))
	  then (!max_delay := decent_trace_len_tmp-cent_trace_len_tmp)
	)										
    done;
  done;
  let r_form = kind
  and r_size_alpha =  string_rep_alphabet alpha
  and r_size_dalpha = string_of_int(List.length (List.hd dalphabets_to_consider))
  and r_cent_trace_len = string_of_float (floor (100.*.(float_of_int(!cent_trace_len) /. float_of_int(!nbtests) /. float_of_int ((List.length dalphabets_to_consider)))) /. 100.)
  and r_cent_num_mess = string_of_float(floor (100.*. float_of_int(!cent_num_mess) /. float_of_int(!nbtests) /. float_of_int ((List.length dalphabets_to_consider))) /. 100.)
  and r_decent_trace_len = string_of_float(floor (100.*. float_of_int(!decent_trace_len) /. float_of_int(!nbtests) /. float_of_int ((List.length dalphabets_to_consider)))/. 100.)
  and r_decent_num_mess = string_of_float(floor (100.*. float_of_int(!decent_num_mess) /. float_of_int(!nbtests) /. float_of_int ((List.length dalphabets_to_consider)))/. 100.) 
  and r_diff_trace_len = string_of_float(floor (10000.*. float_of_int(!decent_trace_len) /.float_of_int(!cent_trace_len) )/. 10000.)
  and r_diff_num_mess = string_of_float(floor (10000.*. float_of_int(!decent_num_mess)/. float_of_int(!cent_num_mess) )/. 10000.)
  and r_delay = string_of_float(floor (10000.*. float_of_int(!delay)/. float_of_int(!delay) )/. 10000.)
  and r_max = string_of_int(!max_delay)
  in
  let r_form = prepare_display r_form 11
  and r_size_alpha = prepare_display r_size_alpha 22
  and r_size_dalpha = prepare_display r_size_dalpha 22
  and r_cent_trace_len = prepare_display r_cent_trace_len 11
  and r_cent_num_mess = prepare_display r_cent_num_mess 9
  and r_decent_trace_len = prepare_display r_decent_trace_len 11
  and r_decent_num_mess = prepare_display r_decent_num_mess 9
  and r_diff_trace_len = prepare_display r_diff_trace_len 11
  and r_diff_num_mess = prepare_display r_diff_num_mess 10
  and r_delay = prepare_display r_delay 8
  and r_max = prepare_display r_max 8

  in
  print_endline("-----------|------------|--------------||-----------|---------||-----------|---------||-----------|----------");
  print_endline(r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"||"^r_delay^"|"^r_max)
**)


let do_pattern_test (name:string) =
  if (!dalphabet_string <> "") then
    (
      perform_test_patterns(name);
    )
      (**
  else
    (
      if (!alphabet_string <> "") then ( for size_dalpha=1 to
	  List.length !alphabet do let dalphabets_to_consider =
				     generate_compatible_dalphabet !alphabet size_dalpha in
				   do_pattern_test_group_results name dalphabets_to_consider !alphabet
	done; )
    )
      **)

let _ =

  welcome();
  
  (* Read the arguments *)
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  Random.self_init();
  
  (* 
     Check that the arguments are correctly passed to the program
  *)
  if (!nbtests <= 0) then (
    print_endline ("Please provide a positive number of tests [-n int]");
    exit(1)
  );
  if (!nb_samples < 0) then (
    print_endline ("Please provide a positive number of samples [-nb_samples int]");
    exit(1)
  );
  if (!nb_samples = 0) then
    (
      nb_samples := !nbtests;
      print_endline ("No number of samples passed to the program. Setting the number of samples to be the number of tests")
    );
  if (not !eval && !sizeform<=0 && !maxsizeform <=0 && not !abscence && not !existence && not !bexistence && not !universality && not !precedence && not !response && not !response_chain && not !precedence_chain && not !constrained_chain) then
    print_endline ("Please provide a positive number for the size of the formula (either [-sf int] or [-msf int]) or a specification pattern to be tested or pass the eval option");
  if (!sizetrace<=0) then
    print_endline ("Please provide a positive number for the size of the trace [-st int]");
  if (!dalphabet_string ="" && !alphabet_string="") then
    print_endline ("Please provide a dalphabet or an alphabet [-dalpha string_representation_of_the_d_alphabet] or [-alpha string_representation_of_the_alphabet]")
  else
    (
      if (!dalphabet_string <> "") then
	dalphabet := parse_dalphabet_string !dalphabet_string;
      if (!alphabet_string <> "") then
	alphabet := parse_alphabet_string !alphabet_string;
    );
  if (!precision < 0 || !precision > 5) then
    (
      print_endline ("Precision should be between 0 and 5 (included).");
      exit (1)
    );
  if (!eval && !alphabet_string = "") then
    (
      print_endline ("The eval option comes with an alphabet");
      exit(1)
    );
  
  (* Check the options for displaying statistics *)
  if (not !eval && (not !print_full_stats && not !print_delay_stats && not !print_trace_and_mess_stats)) then
    print_endline("Please provide some statistics to be displayed: either -prt_trace_mess [bool] or -prt_delay [bool] or -prt_full [bool]");

  (* If one of the options has failed, then exit *)
  if (!nbtests<=0 || (!sizeform<=0 && !maxsizeform <=0 && not !eval && not !abscence && not !existence && not !bexistence && not !universality && not !precedence && not !response && not !response_chain && not !precedence_chain && not !constrained_chain) || !sizetrace<=0 || (!dalphabet_string ="" && !alphabet_string="") || (not !print_full_stats && not !print_delay_stats && not !print_trace_and_mess_stats)) then
    (
      print_endline ("One of the option failed. Exiting.");
      exit(1)
    );
  

  (* Recall the options requested by the user in an explicit manner *)
  if (!eval || !sizeform >0 || !maxsizeform > 0 || !abscence || !existence || !universality || !precedence|| !response || !response_chain || !precedence_chain || !constrained_chain) then
    (
      if (!eval) then
	(
	print_endline ("For each size of distributed alphabet, number of formulae tested: "^string_of_int !nbtests);
	)
      else
	(
	  print_endline("For each entry line, the target number of samples is: "^string_of_int !nbtests);
	  print_endline("For each entry line, the maximal number of tries is: "^string_of_int !nb_samples)
	)
    );
  if (!sizeform >0) then
    print_endline("Running benchmarks for formulae of size: "^string_of_int !sizeform);
  if (!maxsizeform >0) then
    print_endline("Running benchmarks for formulae of maximum size: "^string_of_int !maxsizeform^" (i.e., it will do a complete bench for each formula size between 1 and "^string_of_int !maxsizeform^")");
  if (!abscence || !existence || !universality || !precedence|| !response || !response_chain || !precedence_chain || !constrained_chain) then (
    print_string("The following LTL specification pattern(s) will be tested: ");
    if (!abscence) then print_string(" abscence ");
    if (!existence) then print_string(" existence ");
    if (!bexistence) then print_string(" bounded_existence ");
    if (!universality) then print_string(" universality ");
    if (!precedence) then print_string(" precedence ");
    if (!response) then print_string(" response ");
    if (!response_chain) then print_string(" response_chain ");
    if (!precedence_chain) then print_string(" precedence_chain ");
    if (!constrained_chain) then print_string(" constrained_chain ");
    print_endline("")
  );
  if (!bias) then
    (
      print_string ("Biasing the generation of formulae");
    );
  if (!sizetrace >0) then
    (
      print_endline("Each formula will be monitored against a freshly randomly generated trace of size: "^string_of_int !sizetrace);
      print_endline ("The probability distribution that will be used for the trace is "^(match !Trace.the_distrib with FLIPCOIN -> "flipcoin" | BERNOUILLI -> "bernouilli" | EXPO -> "exponential" | BETA -> "beta"))
    );
  if (!dalphabet_string <>"") then
    print_endline("Distributed alphabet used: "^ !dalphabet_string);

  if (!alphabet_string <>"") then
    print_endline("Alphabet used: "^ !alphabet_string);
  
  if (!Common_test.mode = SEND_CHANGES)	then
    print_endline("Components send the value of their propositions only if there is a change in its value");

  if (!print_full_stats) then
    print_endline("Full statistics will be displayed.");
  if (!print_delay_stats) then
    print_endline("Delay statistics will be displayed.");
  if (!print_trace_and_mess_stats) then
    print_endline("Trace length and messages statistics will be displayed.");

  if (!keep_samples) then
    (
      if (!file_name = "") then
	(
	  file_name := "RESULTS.txt"
	);
      print_endline ("Storing the results in "^(!file_name));
    );

  (* Run benchmarks and display results *)
  print_endline ("Average values will be displayed");
  print_endline ("Values between parenthesis are standard deviations");
  print_endline ("Values between brackets are trace insensitive values");
  print_endline("\nResults:\n");


  if (!eval) then
    (
      if (!maxsizeform > 0) then
	for sizeform_tmp=1 to !maxsizeform do
	  nbcomp_depth_test(sizeform_tmp)	  
	done;
      if (!sizeform > 0) then
	(
	  nbcomp_depth_test(!sizeform)
	);
      exit(1)
    );
  
  if (!keep_samples) then
    (
      write_to_file !file_name ("x cent_trace_len decent_trace_len odecent_trace_len cent_num_mess decent_num_mess odecent_num_mess cent_size_mess decent_size_mess odecent_size_mess cent_nb_progressions decent_nb_progressions odecent_nb_progressions")
    );
  
  if (!sizeform>0 || !maxsizeform >0) then (
    print_header();
    if (!dalphabet_string <> "") then
      (
	if (!maxsizeform > 0) then
	  for sizeform_tmp=1 to !maxsizeform do
	    perform_test(sizeform_tmp)	  
	  done;
	if (!sizeform > 0) then
	  (
	    perform_test(!sizeform)
	  )
      );
    if (!alphabet_string <> "") then
      (
	for size_dalpha=1 to List.length !alphabet do
	  let dalphabets_to_consider = generate_compatible_dalphabet !alphabet size_dalpha in
	  for cpt=0 to (List.length dalphabets_to_consider)-1 do
	    dalphabet:= List.nth dalphabets_to_consider cpt;
	    if (!maxsizeform > 0) then
	      for sizeform_tmp=1 to !maxsizeform do
		perform_test(sizeform_tmp)
	      done;
	    if (!sizeform > 0) then 
	      perform_test(!sizeform)
	  done
	done
      )
  )
  else (

    if (!dalphabet_string <> "") then print_header_patterns();
    if (!alphabet_string <> "") then print_header_patterns_alpha();
    if (!abscence) then (
      do_pattern_test("abs");
    );
    if (!existence) then (
      do_pattern_test("exist");
    );
    if (!bexistence) then (
      do_pattern_test("bexist");
    );
    if (!universality) then (
      do_pattern_test("unive");
    );
    if (!precedence) then (
      do_pattern_test("prec");
    );
    if (!response) then (
      do_pattern_test("resp");
    );
    if (!precedence_chain) then (
      do_pattern_test("pchain");
    );	  	
    if (!response_chain) then (
      do_pattern_test("rchain");
    );
    if (!constrained_chain) then (
      do_pattern_test("cchain");
    );
  )
