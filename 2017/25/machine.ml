open Core

type state = A | B | C | D | E | F

type t = { tape: int Int.Map.t; current_state:state; cursor:int; }

let current_value t =
  Int.Map.find t.tape t.cursor
  |> Option.value ~default:0

let transition t =
  let aux state value =
    match state with
    | A -> if value = 0 then (1, 1, B) else (0, -1, C)
    | B -> if value = 0 then (1, -1, A) else (1, 1, D)
    | C -> if value = 0 then (0, -1, B) else (0, -1, E)
    | D -> if value = 0 then (1, 1, A) else (0, 1, B)
    | E -> if value = 0 then (1, -1, F) else (1, -1, C)
    | F -> if value = 0 then (1, 1, D) else (1, 1, A)
  in
  let data, shift, next = aux t.current_state (current_value t) in
  let tape = Int.Map.add t.tape ~key:t.cursor ~data in
  { tape; current_state=next; cursor=(t.cursor + shift); }