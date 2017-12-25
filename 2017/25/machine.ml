open Core

type state = A | B | C | D | E | F

type t = { tape:Int.Set.t; current_state:state; cursor:int; }

let current_value t =
  Int.Set.mem t.tape t.cursor

let transition t =
  let aux t =
    let on = Int.Set.mem t.tape t.cursor in
    match t.current_state with
    | A -> if not on then (Int.Set.add, 1, B) else (Int.Set.remove, -1, C)
    | B -> if not on then (Int.Set.add, -1, A) else (Int.Set.add, 1, D)
    | C -> if not on then (Int.Set.remove, -1, B) else (Int.Set.remove, -1, E)
    | D -> if not on then (Int.Set.add, 1, A) else (Int.Set.remove, 1, B)
    | E -> if not on then (Int.Set.add, -1, F) else (Int.Set.add, -1, C)
    | F -> if not on then (Int.Set.add, 1, D) else (Int.Set.add, 1, A)
  in
  let update, shift, next = aux t in
  let tape = update t.tape t.cursor in
  { tape; current_state=next; cursor=(t.cursor + shift); }