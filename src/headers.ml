let print_full_stats = ref false
let print_delay_stats = ref false
let print_trace_and_mess_stats = ref false
let print_size_mess = ref true

let cell_size_small = 7 and cell_size_medium = 9 and cell_size_large = 12

let rec spaces (nb:int) : string =
  if (nb <= 0) then ""
  else " "^(spaces (nb-1))

let spaces_small = spaces cell_size_small and spaces_medium = spaces cell_size_medium and spaces_large = spaces cell_size_large

let parenthesize (s:string) : string = "("^s^")"
let parenthesize2 (s:string) : string = "["^s^"]"

let welcome () = 
  print_endline("######################################################");
  print_endline("############ D E C E N T   M O N I T O R #############");
  print_endline("######################################################\n")
 
let print_separation_line_eval () =
  print_endline "-------|-------|-------||---------|---------|---------|---------||---------|---------|---------|---------||---------|---------|---------|---------||"

let print_separator0 () =
  print_endline("--------------------------------||------------------------------"^(if (!print_size_mess) then "---------" else "")^"||------------------------------"^(if (!print_size_mess) then "---------" else "")^"||----------------------------"^(if (!print_size_mess) then "-----------" else "")^"||------------------------------"^(if (!print_size_mess) then "---------" else "")^"||----------------------------"^(if (!print_size_mess) then "-----------" else "")^"||-----------------||-----------------")

let print_separator_pattern0 () =
  print_endline("--------------------------------||-------------------------"^(if (!print_size_mess) then "--------------" else "")^"||------------------------------"^(if (!print_size_mess) then "---------" else "")^"||------------------------------"^(if (!print_size_mess) then "---------" else "")^"||---------------------------"^(if (!print_size_mess) then "------------" else "")^"||---------------------------"^(if (!print_size_mess) then "------------" else "")^"||-----------------||-----------------")



let print_header0()=
  if (!print_size_mess) then
    (
      print_endline ("                                ||            Centralized Case           ||           Decentralized Case          ||           Decentralized Case          ||                Ratio                  ||                 Ratio                 ||      Delays     ||      Delays     ");
      print_endline ("                                ||             (Orchestration)           ||               Migration               ||              Choreography             ||               Migration               ||              Choreography             ||       Migr.     ||      Choreo.    ")
    )
  else
    (
      print_endline ("                                                ||   Centralized Case  ||  Decentralized Case ||  Decentralized Case ||         Ratio         ||         Ratio         ||   Delays    ||    Delays   ");
      print_endline ("                                                ||    (Orchestration)  ||      Migration      ||    Orchestration    ||       Migration       ||      Orchestration    ||    Migr.    ||    Choreo.  ")
    )

 
let print_header() = (
  if (!print_full_stats) then
    (
      print_header0();
      print_separator0();
      print_endline("|form||   alpha.   |  d_alpha.  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||   Avg   |  Max  "^"||   Avg   |  Max  ");
      print_separator0()
    );
  if (!print_delay_stats) then (
    print_endline("                                ||    Delays     ||    Delays     ");
    print_endline("                                ||  (Migration)  || (Choreography)");
    print_endline("--------------------------------||---------------||---------------");
    print_endline("|form||   alpha.   |  d_alpha.  ||  Avg  |  Max  ||  Avg  |  Max  ");
    print_endline("--------------------------------||---------------||---------------")
  );
  if (!print_trace_and_mess_stats) then (
    print_endline("                                ||   Centralized Case    ||  Decentralized Case   ||  Decentralized Case   ||         Ratio         ||          Ratio        ");
    print_endline("                                ||   (Orchestration)     ||       Migration       ||     Chorerography     ||       Migration       ||       Choreography    ");
    print_endline("--------------------------------||-----------------------||-----------------------||-----------------------||-----------------------||------------------------");
    print_endline("|form||   alpha.   |  d_alpha.  |||trace|| #msg  | |msg| |||trace|| #msg  | |msg| |||trace|| #msg  | |msg| |||trace|| #msg  | |msg| |||trace|| #msg  | |msg|  ");
    print_endline("--------------------------------||-----------------------||-----------------------||-----------------------||-----------------------||------------------------")
  );
)


let print_header_patterns() = (
  if (!print_full_stats) then
    (
      print_endline ("                                ||            Centralized Case           ||           Decentralized Case          ||           Decentralized Case          ||                 Ratio                 ||                 Ratio                 ||      Delays     ||      Delays     ");
      print_endline ("                                ||             (Orchestration)           ||              (Migration)              ||             (Choreography)            ||               Migration               ||              Choreography             ||       Migr.     ||      Choreo.    ");
      print_separator_pattern0 ();
      print_endline("pattern|  alpha.   |  d_alpha.  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||  |tr.|  |  # msg  "^(if (!print_size_mess) then "|  |msg|  " else "")^"|  #prog  ||   Avg   |  Max  "^"||   Avg   |  Max  ");
      print_separator_pattern0 ()
    );
  if (!print_delay_stats) then
    (
      print_endline("                                                         ||     Delays  ");
      print_endline("---------------------------------------------------------||---------------");
      print_endline("  patterns |       alphabet       |      d_alphabet      ||   Avg  | Max ");
      print_endline("---------------------------------------------------------||---------------")
    );
  if (!print_trace_and_mess_stats) then
    (
      print_endline("                                                         ||         Centralized Case      ||       Decentralized Case      ||              Ratio                ");
      print_endline("---------------------------------------------------------||-------------------------------||-------------------------------||-----------------------------------");
      print_endline("  pattern  |       alphabet       |      d_alphabet      ||  |trace|  |  # msg  |  |msg|  ||  |trace|  |  # msg  |  |msg|  ||  |trace|  |   # msg    |  |msg|   ");
      print_endline("---------------------------------------------------------||-------------------------------||-------------------------------||-----------------------------------")
    );
)

let print_header_patterns_alpha() = 
print_endline("                                                     ||         Centralized Case      ||       Decentralized Case      ||              Ratio                ");
print_endline("-----------------------------------------------------||-------------------------------||-------------------------------||----------------------------------");
print_endline("  pattern  |      alphabet      |    |d_alphabet|    ||  |trace|  |  # msg  |  |msg|  ||  |trace|  |  # msg  |  |msg|  ||  |trace|  |   # msg    |  |msg|   ");
print_endline("-----------------------------------------------------||-------------------------------||-------------------------------||----------------------------------")
