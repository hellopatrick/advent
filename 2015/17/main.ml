open Core

let sum = List.fold ~init:0 ~f:Int.(+)
let pred l = (sum l) = 150

let rec superset = function
  | [] -> [[]]
  | x::xs ->
    let super = superset xs in
    List.append super (List.map super ~f:(fun set -> x::set))

let find_fewest ls = 
  let f (min, ls) l =
    let len = List.length l in 
    if len < min then (len, [l])
    else if len = min then (min, l::ls)
    else (min, ls)
  in
  List.fold ls ~init:(Int.max_value, []) ~f

let parse_input () =
  In_channel.read_lines "./input.txt"
  |> List.map ~f:Int.of_string

let _ =
  let containers = parse_input () in
  let perfect = superset containers |> List.filter ~f:pred in 
  printf "perfect: %d\n" (List.length perfect);

  let shortest, list = find_fewest perfect in
  printf "fewest & perfect: %d\n" (List.length list);

