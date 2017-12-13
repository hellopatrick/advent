open Core

module Layer = struct
  type direction = Up | Down
  type t = { n:int; depth:int; current:int; dir:direction; }

  let is_zero t = t.current = 0

  let increment_time layer =
    let move_down layer =
      let next = layer.current + 1 in
      if next = layer.depth then {layer with current = (layer.current - 1); dir = Up; }
      else {layer with current = (layer.current + 1); }
    and move_up layer =
      if layer.current = 0 then {layer with current = (layer.current + 1); dir = Down; }
      else {layer with current = (layer.current - 1); }
    in
    match layer.dir with
    | Down -> move_down layer
    | Up -> move_up layer
end

let next_step map =
  Int.Map.map map ~f:Layer.increment_time

let traverse map =
  let until, _ = Int.Map.max_elt_exn map in
  let rec aux map step caught =
    if step > until || not (List.is_empty caught) then caught
    else
      let caught = match Int.Map.find map step with
        | Some layer -> if Layer.is_zero layer then layer::caught else caught
        | None -> caught
      in
      let new_map = next_step map in
      aux new_map (step+1) caught
  in aux map 0 []

let find_safe_time map =
  let rec aux map n =
    let map = next_step map in
    match traverse map with
    | [] -> n
    | _ -> aux map (n+1)
  in aux map 1

let parse_inputs () =
  let parse_line line =
    let to_layer = function
      | [n; depth] -> Some Layer.{n; depth; current=0; dir=Down}
      | _ -> None
    in
    String.split line ~on:':'
    |> List.map ~f:(Fn.compose Int.of_string String.strip)
    |> to_layer
  in
  let open Layer in
  In_channel.read_lines "./input.txt"
  |> List.filter_map ~f:parse_line
  |> List.fold ~init:(Int.Map.empty) ~f:(fun acc layer -> Map.add acc ~key:layer.n ~data:layer)

let _ =
  let input = parse_inputs () in
  let brute = find_safe_time input in
  printf "b: %d\n" brute;