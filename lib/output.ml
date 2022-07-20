
let rec gen_space (n : int) =
  if (n <= 0) then ""
  else " " ^ gen_space (n - 1)

let adjust_string (s : string) (n : int) =
  let l = String.length s in
    if l < n then s ^ (gen_space (n - l)) else s

let gen_cell (s : string) (n : int) =
  let half = (n - (String.length s)) / 2 in
    gen_space half ^ s ^ gen_space half


let prepare_display (s : string) (n : int) : string =
  adjust_string (gen_cell s n) n

let round f = Utils.round_generic f !Settings.precision

let rounded_string x = string_of_float (round x)

let prepare_display_and_round f n = prepare_display (rounded_string f) n

let prepare_display_no_round f n = prepare_display (string_of_float f) n
