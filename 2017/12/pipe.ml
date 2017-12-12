open Core

type t = { n:int; links:int list}

let to_string t =
  sprintf "%d | %s" t.n (List.to_string (Int.to_string) t.links)