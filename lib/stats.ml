


type 'a value_stats = {
  mutable single: 'a;
  mutable avg: float;
  mutable var: float;
  mutable trin: float;
}

type trace_info = { 
  mutable len: int value_stats;  
  mutable num_mess: int value_stats;
  mutable size_mess: float value_stats;
  mutable nb_progressions: int value_stats;
}

let create_value_stats (init : 'a) = {
  single = init;
  avg = 0.0;
  var = 0.0;
  trin = 0.0;
}

let create_trace_info () = {
  len = create_value_stats(0);
  num_mess = create_value_stats(0);
  size_mess = create_value_stats(0.0);
  nb_progressions = create_value_stats(0);
}

let reset (info: trace_info) = 
  info.len <- create_value_stats(0);
  info.num_mess <- create_value_stats(0);
  info.size_mess <- create_value_stats(0.0);
  info.nb_progressions <- create_value_stats(0);
  