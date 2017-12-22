type t = Clean | Weakened | Infected | Flagged

let next_state = function
  | Clean -> Weakened
  | Weakened -> Infected
  | Infected -> Flagged
  | Flagged -> Clean