open Core

module Recipe = struct
  type t = (Ingredient.t * int) list
  [@@deriving sexp]

  let score t =
    let capacity = List.fold t ~init:0 ~f:(fun acc (i, amt) -> acc + (amt * Ingredient.capacity i))
    and durability = List.fold t ~init:0 ~f:(fun acc (i, amt) -> acc + (amt * Ingredient.durability i))
    and flavor = List.fold t ~init:0 ~f:(fun acc (i, amt) -> acc + (amt * Ingredient.flavor i))
    and texture = List.fold t ~init:0 ~f:(fun acc (i, amt) -> acc + (amt * Ingredient.texture i)) in
    List.map [capacity; durability; flavor; texture] ~f:(Int.max 0)
    |> List.fold ~init:1 ~f:Int.( * )

  let is_meal_replacement t =
    let total_calories = List.fold t ~init:0 ~f:(fun acc (i, amt) -> acc + (amt * Ingredient.calories i))
    in total_calories = 500

  let compare a b =
    Int.compare (score a) (score b)

  let to_string t =
    let sexp = sexp_of_t t in
    sprintf "%s -> %d" (Sexp.to_string_hum sexp) (score t)
end

let sortings bins total  =
  let rec aux bins n l =
    match bins with
    | 0 -> [l]
    | 1 -> aux 0 0 (n::l)
    | k ->
      let rec loop j res =
        if j >= 0 then loop (j-1) (List.append (aux (k-1) (n-j) (j::l)) res)
        else res
      in loop n []
  in aux bins total []

let parse_input () =
  In_channel.read_lines "./input.txt"
  |> List.filter_map ~f:Ingredient.of_string

let _ =
  let ingredients = parse_input () in
  let combos = sortings (List.length ingredients) 100 in
  let recipes = List.map combos ~f:(List.zip_exn ingredients) in
  let low_cal_recipes = List.filter recipes ~f:(Recipe.is_meal_replacement) in
  match List.max_elt low_cal_recipes ~cmp:Recipe.compare with
  | Some best -> printf "best: %s\n" (Recipe.to_string best)
  | None -> printf "nothing good.\n"