type t = Clean | Weakened | Infected | Flagged

let next_state = function
  | Clean -> Weakened
  | Weakened -> Infected
  | Infected -> Flagged
  | Flagged -> Clean

let of_char = function
  | '#' -> Infected
  | '.' -> Clean
  | 'W' -> Weakened
  | 'F' -> Flagged
  | _ -> failwith "not a supported state"