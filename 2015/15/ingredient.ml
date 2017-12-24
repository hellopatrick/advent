open Core

module T = struct
  type t = { name:string;
             capacity:int;
             durability:int;
             flavor:int;
             texture:int;
             calories:int; }
  [@@deriving sexp, compare, fields]
end

include T
include Comparable.Make(T)

let of_string str =
  let split = String.split_on_chars str ~on:[' '; ','; ':'] in
  let words = List.filter split ~f:(fun s -> not (String.is_empty s)) in
  match words with
  | [name; _; capacity; _; durability; _; flavor; _; texture; _; calories; ] ->
    let capacity = Int.of_string capacity
    and durability = Int.of_string durability
    and flavor = Int.of_string flavor
    and texture = Int.of_string texture
    and calories = Int.of_string calories in
    Some { name; capacity; durability; flavor; texture; calories; }
  | _ -> None