open Core

module T = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end

let of_string str =
  match String.split str ~on:'/' with
  | [i; o;] ->
    let i = Int.of_string i
    and o = Int.of_string o in (i, o)
  | _ -> failwith "error parsing port."

let is_magnetic = function
  | (0, _) | (_, 0) -> true
  | _ -> false

let can_connect p (a, b) = a = p || b = p

let opposite p (a, b) =
  if p = a then b
  else a

let strength (a, b) = a + b

include T