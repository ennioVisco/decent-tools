

let precision = ref 3


let seed = ref 0
let enforce = ref false
let optimistic = ref false
let print_debug = ref false
let produce_tex = ref false
let formula_file_name = ref ""
let nb_formula = ref 0
let formula_list : Ltl.ltl list ref = ref []
let nbtests = ref 0
let sizeform = ref 0
let maxsizeform = ref 0
let sizetrace = ref 0
let dalphabet_string = ref ""
let dalphabet : Alphabetevent.alphabet list ref = ref []
let alphabet_string = ref ""
let alphabet : string list ref = ref []
let multi_alpha_f = ref ""
let n_alpha = ref 0
let dalphabet_list : Alphabetevent.d_alphabet list ref = ref []
let abscence = ref false
let existence = ref false
let bexistence = ref false
let universality = ref false
let response = ref false
let precedence_chain = ref false
let precedence = ref false
let response_chain = ref false
let constrained_chain = ref false
let bias = ref false
let eval = ref false
let file_name = ref ""
let stat_file_name = ref ""
let keep_samples = ref false
let nb_samples = ref 0
let counter_tests = ref 0
let counter_samples = ref 0

let iteration_counter = ref 0