open Core

type t = {mem: int String.Map.t; max: int}

let get mem k =
  match Map.find mem k with
  | Some v -> v
  | None -> 0

let perform' {mem; max} key f v =
  let current = get mem key in
  let data = f current v in
  let mem = Map.add mem ~key ~data in
  let max = if data > max then data else max in
  {mem; max}

let perform state =
  let open Instruction in function
    | Increment (n, v) -> perform' state n Int.(+) v
    | Decrement (n, v) -> perform' state n Int.(-) v

let verify mem =
  let get = get mem in
  let open Instruction in function
    | GreaterThan (n, v) -> (get n) > v
    | GreaterThanOrEqual (n, v)-> (get n) >= v
    | LessThan (n, v) -> (get n) < v
    | LessThanOrEqual (n, v) -> (get n) <= v
    | Equal (n, v) -> (get n) = v
    | NotEqual (n, v) -> (get n) <> v

let run ({mem; _} as state) Instruction.{action; condition} =
  if verify mem condition then perform state action
  else state