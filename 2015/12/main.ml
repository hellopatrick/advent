open Core

let input () =
  Yojson.Basic.from_file "./input.txt"

let rec sum json n =
  match json with
  | `Assoc kv -> 
    n + List.fold kv ~init:0 ~f:(fun s (_, j) -> s + sum j 0)
  | `List v ->
    n + List.fold v ~init:0 ~f:(fun s j -> s + sum j 0)
  | `Int i -> i
  | _ -> 0

let _ =
  let json = input () in
  let sum = sum json 0 in
  printf "%d\n" sum