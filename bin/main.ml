open Batteries
open DynArray
open Decent
open Alphabetevent
open Alphabet_parser
open Trace
open Ltl
open Ltl_parser
open Architecture
open Common_test
open Config
open Headers
open Utils
open Output
open Settings

let cent_trace_info = Stats.create_trace_info ()
let decent_trace_info = Stats.create_trace_info ()
let odecent_trace_info = Stats.create_trace_info ()

let cent_trace_len = ref 0 and decent_trace_len = ref 0 and odecent_trace_len = ref 0 and cent_num_mess = ref 0 and decent_num_mess = ref 0 and odecent_num_mess = ref 0 and cent_size_mess = ref 0. and decent_size_mess = ref 0. and odecent_size_mess = ref 0. and cent_nb_progressions = ref 0  and decent_nb_progressions = ref 0 and odecent_nb_progressions = ref 0

let cent_trace_len_avg = ref 0. and decent_trace_len_avg = ref 0. and odecent_trace_len_avg = ref 0. and cent_num_mess_avg = ref 0. and decent_num_mess_avg = ref 0. and odecent_num_mess_avg = ref 0. and cent_size_mess_avg = ref 0. and decent_size_mess_avg = ref 0. and odecent_size_mess_avg = ref 0. and cent_nb_progressions_avg = ref 0.  and decent_nb_progressions_avg = ref 0. and odecent_nb_progressions_avg = ref 0. and delay_avg = ref 0. and odelay_avg = ref 0.

let cent_trace_len_var = ref 0. and decent_trace_len_var = ref 0. and odecent_trace_len_var = ref 0. and cent_num_mess_var = ref 0. and decent_num_mess_var = ref 0. and odecent_num_mess_var = ref 0. and cent_size_mess_var = ref 0. and decent_size_mess_var = ref 0. and odecent_size_mess_var = ref 0. and cent_nb_progressions_var = ref 0.  and decent_nb_progressions_var = ref 0. and odecent_nb_progressions_var = ref 0. and delay_var = ref 0. and odelay_var = ref 0.

let cent_num_mess_trin = ref 0. and decent_num_mess_trin = ref 0. and odecent_num_mess_trin = ref 0. and cent_size_mess_trin = ref 0. and decent_size_mess_trin = ref 0. and odecent_size_mess_trin = ref 0. and cent_nb_progressions_trin = ref 0.  and decent_nb_progressions_trin = ref 0. and odecent_nb_progressions_trin = ref 0.



let delay = ref 0 and odelay = ref 0
let formula = ref False

let max_delay = ref 0 and max_odelay = ref 0

let usage = "usage: " ^ Sys.argv.(0) ^ " [-n int]"

let speclist = [
  ("-seed", Arg.Int   (fun s -> seed := s), "int (<> 0) : sets a seed [Enf]");
  ("-enforce", Arg.Bool   (fun enf -> enforce := enf),  "bool : do enforcement instead of monitoring [Enf] ");
  ("-optimistic", Arg.Bool   (fun alw_enf -> optimistic := alw_enf; Common_test.always_enforce := not alw_enf), "bool : use the \"optimistic\" mode (always compute the alternatives regardless of whether there is a violation or not)");
  ("-specific_f", Arg.String   (fun f_name -> formula_file_name := f_name),   "filename : file containing specific formulas to enforce [Enf]");
  ("-nf", Arg.Int   (fun nf -> nb_formula := nf),  "int : number of specific formulas to enforce given in the file [Enf]");
  ("-n", Arg.Int    (fun n -> nbtests := n),  "int : the maximum number of tests to run to obtain one row benchmark (each test can result in a meaningful sample or not, e.g., in the case of a non-monitorable formula) [Mon/Enf]");
  ("-sf", Arg.Int    (fun sf -> sizeform := sf),      "int : the size of the formula [Mon/Enf]");
  ("-msf", Arg.Int    (fun msf -> maxsizeform := msf),      "int : the maximum size of the formula (will test from size 1 to the value provided) [Mon/Enf]");
  ("-st", Arg.Int    (fun st -> sizetrace := st),      "int : the size of the trace [Mon/Enf]");
  ("-dalpha", Arg.String  (fun dalpha -> dalphabet_string := dalpha),     "string : the decentralized alphabet [Mon/Enf]" );
  ("-alpha", Arg.String  (fun alpha -> alphabet_string := alpha),     "string : the centralized alphabet (will consider possible dalphabets generated from it) [Mon/Enf]");
  ("-multi_dalpha", Arg.String  (fun ma_f -> multi_alpha_f := ma_f),  "filename : file containing multiples decentralized alphabet [Enf]");
  ("-n_alpha", Arg.Int  (fun na -> n_alpha := na),  "int : number of decentralized alphabet given in the file [Enf]");
  ("-abs", Arg.Bool  (fun abs -> abscence := abs),     "bool : use absence patterns [Mon/Enf]");
  ("-exis", Arg.Bool  (fun exis -> existence := exis),     "bool : use  existence patterns [Mon/Enf]");
  ("-bexis", Arg.Bool  (fun bexis -> bexistence := bexis),     "bool : use  bounded existence patterns [Mon/Enf]");
  ("-univ", Arg.Bool  (fun univ -> universality := univ),     "bool : use universality patterns [Mon/Enf]");
  ("-prec", Arg.Bool  (fun prec -> precedence := prec),     "bool : use precedence patterns [Mon/Enf]");
  ("-resp", Arg.Bool  (fun resp -> response := resp),     "bool : use response patterns [Mon/Enf]");
  ("-precc", Arg.Bool  (fun precc -> precedence_chain := precc),     "bool : use precedence chain patterns [Mon/Enf]");
  ("-respc", Arg.Bool  (fun respc -> response_chain:= respc),     "bool : use response chain patterns [Mon/Enf]");
  ("-consc", Arg.Bool  (fun consc -> constrained_chain:= consc),     "bool : use constrained chain patterns [Mon/Enf]");
  ("-prt_trace_mess", Arg.Bool  (fun prttracemess -> print_trace_and_mess_stats := prttracemess),     "bool : print trace and number of messages statistics [Mon]");
  ("-prt_delay", Arg.Bool  (fun prtdelay -> print_delay_stats := prtdelay),     "bool : print delay statistics [Mon]");
  ("-prt_full", Arg.Bool  (fun prtfull -> print_full_stats := prtfull),     "bool : print full statistics [Mon]");
  ("-prt_full_stats", Arg.Bool (fun prtdebug -> print_debug := prtdebug), "bool : print statistics of all the enforcement runs [Enf]");
  ("-flipcoin", Arg.Unit (fun x -> Trace.the_distrib := FLIPCOIN), ": use the flipcoin probability distribution (uniform distribution with probability 0.5) [Mon/Enf]");
  ("-bernouilli", Arg.Float (fun seed -> Trace.the_distrib := BERNOUILLI; Trace.seed := seed), "float : use the BERNOUILLI probability distribution (uniform distribution with a probability given as an argument) [Mon/Enf]");
  ("-expo", Arg.Float (fun seed -> Trace.the_distrib := EXPO; Trace.seed := seed), "float : use the EXPONENTIAL probability distribution (the rate parameter is given as an argument) [Mon/Enf]");
  ("-beta", Arg.Tuple [Arg.Float (fun seed -> Trace.the_distrib := BETA; Trace.seeda := seed);
                       Arg.Float (fun seed -> Trace.seedb := seed)], "float float: use the BETA probability distribution (the rate parameters are given as arguments) [Mon/Enf]");
  ("-only_changes", Arg.Unit  (fun x -> Config.mode := SEND_CHANGES),": components send the value of their propositions only if there is a change in its value. More precisely, if among the monitors, there is at least the value of one proposition that is modified wrt the previous event, the component has to send a message to the monitor [Mon]");
  ("-bias", Arg.Unit    (fun _ -> bias := true),  ": bias the generation of formulae to favor one component [Mon/Enf] ");
  ("-precision", Arg.Int    (fun p -> precision := p),  "int : precision of numbers (number of decimals) [Mon/Enf]");
  ("-eval", Arg.Unit    (fun p -> eval := true),  ": uses every distributed alphabets that can be built from the given alphabet once [Mon/Enf]");
  ("-keep_samples", Arg.Bool  (fun keep -> keep_samples := keep),     "bool : keep samples [Mon/Enf]");
  ("-prod_tex", Arg.Bool  (fun tex -> produce_tex := tex),  "bool : produces additional output files containing results usable for a LaTeX table (requires keep_samples) [Enf]");
  ("-file", Arg.String  (fun file -> file_name := file),     "filename : file on which samples should be stored [Mon/Enf]");
  ("-nb_samples", Arg.Int  (fun nb -> nb_samples := nb),     "int : the number of target samples to obtain [Mon]");

]

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
    cent_trace_len_tmp_string = prepare_display_and_round cent_trace_len_tmp cell_size_medium and
    decent_trace_len_tmp_string = prepare_display_and_round decent_trace_len_tmp cell_size_medium and
    odecent_trace_len_tmp_string = prepare_display_and_round odecent_trace_len_tmp cell_size_medium and
    cent_num_mess_tmp_string = prepare_display_and_round cent_num_mess_tmp cell_size_medium and
    decent_num_mess_tmp_string = prepare_display_and_round decent_num_mess_tmp cell_size_medium and
    odecent_num_mess_tmp_string = prepare_display_and_round odecent_num_mess_tmp cell_size_medium and
    cent_size_mess_tmp_string = prepare_display_and_round cent_size_mess_tmp cell_size_medium and
    decent_size_mess_tmp_string = prepare_display_and_round decent_size_mess_tmp cell_size_medium and
    odecent_size_mess_tmp_string = prepare_display_and_round odecent_size_mess_tmp cell_size_medium and
    cent_nb_progressions_tmp_string = prepare_display_and_round cent_nb_progressions_tmp cell_size_medium and
    decent_nb_progressions_tmp_string = prepare_display_and_round decent_nb_progressions_tmp cell_size_medium and
    odecent_nb_progressions_tmp_string = prepare_display_and_round odecent_nb_progressions_tmp cell_size_medium and
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
  and r_cent_trace_len = prepare_display r_cent_trace_len cell_size_small
  and r_cent_trace_len_var = prepare_display r_cent_trace_len_var cell_size_small
  and r_cent_num_mess = prepare_display r_cent_num_mess cell_size_small
  and r_cent_num_mess_var = prepare_display r_cent_num_mess_var cell_size_small
  and r_cent_num_mess_trin = prepare_display r_cent_num_mess_trin cell_size_small
  and r_cent_size_mess = prepare_display r_cent_size_mess cell_size_small
  and r_cent_size_mess_var = prepare_display r_cent_size_mess_var cell_size_small
  and r_cent_size_mess_trin = prepare_display r_cent_size_mess_trin cell_size_small
  and r_cent_nb_progressions = prepare_display r_cent_nb_progressions cell_size_small
  and r_cent_nb_progressions_var = prepare_display r_cent_nb_progressions_var cell_size_small
  and r_cent_nb_progressions_trin = prepare_display r_cent_nb_progressions_trin cell_size_small
  and r_decent_trace_len = prepare_display r_decent_trace_len cell_size_small
  and r_decent_trace_len_var = prepare_display r_decent_trace_len_var cell_size_small
  and r_decent_num_mess = prepare_display r_decent_num_mess cell_size_small
  and r_decent_num_mess_var = prepare_display r_decent_num_mess_var cell_size_small
  and r_decent_num_mess_trin = prepare_display r_decent_num_mess_trin cell_size_small
  and r_decent_size_mess = prepare_display r_decent_size_mess cell_size_small
  and r_decent_size_mess_var = prepare_display r_decent_size_mess_var cell_size_small
  and r_decent_size_mess_trin = prepare_display r_decent_size_mess_trin cell_size_small
  and r_decent_nb_progressions = prepare_display r_decent_nb_progressions cell_size_small
  and r_decent_nb_progressions_var = prepare_display r_decent_nb_progressions_var cell_size_small
  and r_decent_nb_progressions_trin = prepare_display r_decent_nb_progressions_trin cell_size_small
  and r_odecent_trace_len = prepare_display r_odecent_trace_len cell_size_small
  and r_odecent_trace_len_var = prepare_display r_odecent_trace_len_var cell_size_small
  and r_odecent_num_mess = prepare_display r_odecent_num_mess cell_size_small
  and r_odecent_num_mess_var = prepare_display r_odecent_num_mess_var cell_size_small
  and r_odecent_num_mess_trin = prepare_display r_odecent_num_mess_trin cell_size_small
  and r_odecent_size_mess = prepare_display r_odecent_size_mess cell_size_small
  and r_odecent_size_mess_var = prepare_display r_odecent_size_mess_var cell_size_small
  and r_odecent_size_mess_trin = prepare_display r_odecent_size_mess_trin cell_size_small
  and r_odecent_nb_progressions = prepare_display r_odecent_nb_progressions cell_size_small
  and r_odecent_nb_progressions_var = prepare_display r_odecent_nb_progressions_var cell_size_small
  and r_odecent_nb_progressions_trin = prepare_display r_odecent_nb_progressions_trin cell_size_small
  and r_diff_trace_len = prepare_display r_diff_trace_len cell_size_small
  and r_diff_num_mess = prepare_display r_diff_num_mess cell_size_small
  and r_diff_size_mess = prepare_display r_diff_size_mess cell_size_small
  and r_diff_nb_progressions = prepare_display r_diff_nb_progressions cell_size_small
  and r_odiff_trace_len = prepare_display r_odiff_trace_len cell_size_small
  and r_odiff_num_mess = prepare_display r_odiff_num_mess cell_size_small
  and r_odiff_size_mess = prepare_display r_odiff_size_mess cell_size_small
  and r_odiff_nb_progressions = prepare_display r_odiff_nb_progressions cell_size_small
  and r_delay = prepare_display r_delay cell_size_small
  and r_delay_var = prepare_display r_delay_var cell_size_small
  and r_odelay = prepare_display r_odelay cell_size_small
  and r_odelay_var = prepare_display r_odelay_var cell_size_small
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
      print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_delay^"|"^r_max^"||"^r_odelay^"|"^r_omax)
    );
  if (!print_trace_and_mess_stats) then
    (
      print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_size_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"|"^r_decent_size_mess^"||"^r_odecent_trace_len^"|"^r_odecent_num_mess^"|"^r_odecent_size_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess^"||"^r_odiff_trace_len^"|"^r_odiff_num_mess^"|"^r_odiff_size_mess)
    );
  print_endline("")

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
      print_endline (r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_decent_size_mess^"||"^r_odecent_trace_len^"|"^r_odecent_num_mess^"|"^r_odecent_size_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess^"||"^r_odiff_trace_len^"|"^r_odiff_num_mess^"|"^r_odiff_size_mess)
    )

let do_pattern_test (name:string) =
  if (!dalphabet_string <> "") then
    (
      perform_test_patterns(name);
    )

let print_stat (s: string) =
  if !file_name <> "" then
    write_to_file !stat_file_name s;
  print_endline s

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

  if (not !enforce && (!seed <> 0 || !formula_file_name <> "" || !nb_formula <> 0 || !multi_alpha_f <> "" || !n_alpha <> 0 || !print_debug <> false)) then (
    print_endline "An unsupported option for monitoring was given to the program";
    exit(1)
  );

  if (!enforce && (!nb_samples <> 0 || !Config.mode <> SEND_EVERYTHING || !print_delay_stats <> false || !print_trace_and_mess_stats <> false || !print_full_stats <> false)) then (
    print_endline "An unsupported option for enforcement was given to the program";
    exit(1)
  );

  if (!formula_file_name = "") then (
    if (!nbtests <= 0) then (
      print_endline ("Please provide a positive number of tests [-n int]");
      exit(1)
    );
    if (not !enforce && !nb_samples < 0) then (
      print_endline ("Please provide a positive number of samples [-nb_samples int]");
      exit(1)
    );
    if (not !enforce && !nb_samples = 0) then
      (
        nb_samples := !nbtests;
        print_endline ("No number of samples passed to the program. Setting the number of samples to be the number of tests")
      );
  ) else (
    if (!nb_formula = 0) then (
      print_endline "Please provide the number of formulas contained in the given file";
      exit(1)
    );

    let specific_formulas_file = open_in !formula_file_name in

    let rec read_formulas (n: int) =
      if n <> 0 then
        let line = input_line specific_formulas_file in
        formula_list := !formula_list @ [parse_formula_string line];
        read_formulas (n-1)

    in read_formulas !nb_formula
  );

  if (!sizeform<=0 && !formula_file_name="" && (!enforce || !maxsizeform <=0) && not !abscence && not !existence && not !bexistence && not !universality && not !precedence && not !response && not !response_chain && not !precedence_chain && not !constrained_chain) then
    print_endline ("Please provide a positive number for the size of the formula (either [-sf int] or [-msf int]) or a specification pattern to be tested");
  if (!sizetrace<=0) then
    print_endline ("Please provide a positive number for the size of the trace [-st int]");
  if (!dalphabet_string ="" && ((!enforce && not !eval) || !alphabet_string="") && (not !enforce || !multi_alpha_f="")) then
    print_endline ("Please provide a dalphabet, an alphabet or a file containing a list of dalphabets: [-dalpha string_representation_of_the_d_alphabet] or [-alpha string_representation_of_the_alphabet] or [-multi_dalpha filename]")
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

  (* Use multiple alphabets *)
  if (!enforce) then (
    if !multi_alpha_f <> "" then (
      if (!n_alpha = 0) then (
        print_endline "Please provide the number of alphabets contained in the file";
        exit(1)
      );

      let m_alpha_file = open_in !multi_alpha_f in 

      let rec read_dalphabets (n :int) = 
        if n <> 0 then
          let line = input_line m_alpha_file in
          dalphabet_list := !dalphabet_list @ [parse_dalphabet_string line];
          read_dalphabets (n - 1)
      
      in read_dalphabets !n_alpha

    ) else (
      dalphabet_list := [!dalphabet]
    )
  );

  (* Check the options for displaying statistics *)
  if (not !enforce && not !eval && (not !print_full_stats && not !print_delay_stats && not !print_trace_and_mess_stats)) then
    print_endline("Please provide some statistics to be displayed: either -prt_trace_mess [bool] or -prt_delay [bool] or -prt_full [bool]");

  (* If one of the options has failed, then exit *)
  if (not !enforce) then (
    if (!nbtests<=0 || !sizetrace<=0 || (!dalphabet_string ="" && !alphabet_string="") || (not !print_full_stats && not !print_delay_stats && not !print_trace_and_mess_stats)) || (!sizeform<=0 && !maxsizeform <=0 && 
        not !abscence && not !existence && not !bexistence && not !universality && not !precedence && not !response && not !response_chain && not !precedence_chain && not !constrained_chain)  then (
      print_endline ("One of the option failed. Exiting.");
      exit(1)
    )
  ) else
    if ((!nbtests<=0 && !formula_file_name = "")  || !sizetrace<=0 || (!dalphabet_string ="" && !multi_alpha_f="" && ((!enforce && not !eval) || !alphabet_string="")) || (!sizeform<=0 && !maxsizeform <=0 &&
      !formula_file_name="" && not !abscence && not !existence && not !bexistence && not !universality && not !precedence && not !response && not !response_chain && not !precedence_chain &&  not !constrained_chain)) then (
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
          print_endline("For each entry line, the maximum number of tests is: "^string_of_int !nbtests);
          if (!nb_samples <> 0) then
            print_endline("For each entry line, the target number of sample is: "^string_of_int !nb_samples)
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
  if (not !enforce && !dalphabet_string <>"") then
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
  if (!optimistic) then
    print_endline("Optimistic mode is used")
  else
    print_endline("Pessimistic mode is used");

  if (!keep_samples) then
    (
      if (!file_name = "") then
        (
          if not !enforce then file_name := "RESULTS_MON.txt"
          else file_name := "RESULTS_ENF.txt"
        );
      (* Remove the file if it already exists, otherwise it appends to the existing file and makes it unusable *)
      if Sys.file_exists !file_name then
        Sys.remove !file_name;
      
      stat_file_name := "STATS_" ^ !file_name;
      
      if Sys.file_exists !stat_file_name then
        Sys.remove !stat_file_name;

      print_endline ("Storing the results in "^(!file_name)^" and the statistics in "^(!stat_file_name));

      if !produce_tex then (
        if Sys.file_exists "STATS_TEX_1.txt" then
          Sys.remove "STATS_TEX_1.txt";

        if Sys.file_exists "STATS_TEX_2.txt" then
          Sys.remove "STATS_TEX_2.txt"
      )
    );

  (* Run benchmarks and display results *)
  if (not !enforce) then (
    print_endline ("Average values will be displayed");
    print_endline ("Values between parenthesis are standard deviations");
    print_endline ("Values between brackets are trace insensitive values")
  );
  print_endline("\nResults:\n");

  if (not !enforce) then
    (
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
          write_to_file !file_name ("x nb_comp nb_ap cent_trace_len decent_trace_len odecent_trace_len cent_num_mess decent_num_mess odecent_num_mess cent_size_mess decent_size_mess odecent_size_mess cent_nb_progressions decent_nb_progressions odecent_nb_progressions")
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
    )
  else if !enforce then (
    if !eval then
      dalphabet_list := generate_all_compatible_dalphabets !alphabet;

    (* Enforce with every alphabet of the list *)
    let rec enforcer (alpha_list: d_alphabet list) =
      print_stat "\n####################################################################################################";
      print_stat "####################################################################################################\n";

      match alpha_list with
          hd::tl -> (
            dalphabet := hd;
            print_stat ("#################### Decentralized alphabet used: " ^ stringrep_dalphabet !dalphabet ^ " ####################\n");

            if (!keep_samples) then
              write_to_file !file_name "x nb_comp nb_ap nb_modif_g nb_modif_l nb_modif_cent nb_msg_g nb_msg_l nb_msg_cent msg_size_g msg_size_l msg_size_cent tcl_size_g tcl_size_l tcl_size_cent";

            if (!sizeform > 0 || !maxsizeform > 0) then (
              print_stat "#######################################";
              print_stat "########### RANDOM FORMULAS ###########";
              print_stat "#######################################";
              if !maxsizeform > 0 then
                for i = 1 to !maxsizeform do
                  print_stat "\n#######################################";
                  print_stat ("############### SIZE "^string_of_int i^" ################");
                  print_stat "#######################################";
                  if !seed = 0 then
                    test_random_formula !nbtests !dalphabet i !sizetrace !bias !precision !print_debug !file_name !produce_tex
                  else
                    test_random_formula_seeded !seed !nbtests !dalphabet i !sizetrace !bias !precision !print_debug !file_name !produce_tex
                done
              else
                if !seed = 0 then
                  test_random_formula !nbtests !dalphabet !sizeform !sizetrace !bias !precision !print_debug !file_name !produce_tex
                else
                  test_random_formula_seeded !seed !nbtests !dalphabet !sizeform !sizetrace !bias !precision !print_debug !file_name !produce_tex
            );

            if (!formula_file_name <> "") then (
              print_stat "#######################################";
              print_stat "########## SPECIFIC FORMULAS ##########";
              print_stat "#######################################";
              
              if !seed = 0 then
                test_specific !formula_list !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else 
                test_specific_seeded !seed !formula_list !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );

            if (!abscence) then (
              print_stat "\n#######################################";
              print_stat "############### ABSENCE ###############";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "abs" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "abs" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );
            if (!existence) then (
              print_stat "\n#######################################";
              print_stat "############## EXISTENCE ##############";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "exist" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "exist" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );
            if (!bexistence) then (
              print_stat "\n#######################################";
              print_stat "########## BOUNDED EXISTENCE ##########";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "bexist" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "bexist" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );
            if (!universality) then (
              print_stat "\n#######################################";
              print_stat "############ UNIVERSALITY #############";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "unive" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "unive" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );
            if (!precedence) then (
              print_stat "\n#######################################";
              print_stat "############# PRECEDENCE ##############";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "prec" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "prec" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );
            if (!response) then (
              print_stat "\n#######################################";
              print_stat "############# RESPONSE ################";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "resp" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "resp" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );
            if (!precedence_chain) then (
              print_stat "\n#######################################";
              print_stat "########## PRECEDENCE CHAIN ###########";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "pchain" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "pchain" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );
            if (!response_chain) then (
              print_stat "\n#######################################";
              print_stat "########### RESPONSE CHAIN ############";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "rchain" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "rchain" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );
            if (!constrained_chain) then (
              print_stat "\n#######################################";
              print_stat "########## CONSTRAINED CHAIN ##########";
              print_stat "#######################################";
              if !seed = 0 then
                test_pattern "cchain" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
              else
                test_pattern_seeded !seed "cchain" !nbtests !dalphabet !sizetrace !precision !print_debug !file_name !produce_tex
            );

            enforcer tl
          )
      | _ -> ()

    in enforcer !dalphabet_list
  )