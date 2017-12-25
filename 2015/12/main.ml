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

let rec sum_no_red json n =
  let has_no_red kv =
    let f (k, json) =
      match json with
      | `String str -> String.equal str "red"
      | _ -> false
    in not (List.exists kv ~f)
  in
  match json with
  | `Assoc kv ->
    if has_no_red kv then n + List.fold kv ~init:0 ~f:(fun s (_, j) -> s + sum_no_red j 0)
    else n
  | `List v ->
    n + List.fold v ~init:0 ~f:(fun s j -> s + sum_no_red j 0)
  | `Int i -> i
  | _ -> 0

let _ =
  let json = input () in
  sum json 0 |> printf "with reds: %d\n";
  sum_no_red json 0 |> printf "with no reds: %d\n";