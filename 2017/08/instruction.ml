type action =
  | Increment of (string * int)
  | Decrement of (string * int)

type condition =
  | GreaterThan of (string * int)
  | GreaterThanOrEqual of (string * int)
  | LessThan of (string * int)
  | LessThanOrEqual of (string * int)
  | Equal of (string * int)
  | NotEqual of (string * int)

type t = { action: action; condition: condition }