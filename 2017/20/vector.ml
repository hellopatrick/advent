open Core

module T = struct
  type t = { x:int; y:int; z:int; }
  [@@deriving sexp, compare, hash]
end

include Comparable.Make(T)
include Hashable.Make(T)
include T

let abs a =
  let x = Int.abs a.x
  and y = Int.abs a.y
  and z = Int.abs a.z
  in {x;y;z}

let compare_manhattan a b =
  T.compare (abs a) (abs b)

let same a b =
  (Int.equal a.x b.x) && (Int.equal a.y b.y) && (Int.equal a.y b.y)

let to_string t =
  sprintf "(%d, %d, %d)" t.x t.y t.z

let add a b =
  let x = a.x + b.x
  and y = a.y + b.y
  and z = a.z + b.z
  in { x; y; z; }