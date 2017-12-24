open Core

let parse_input () =
  In_channel.read_lines "./input.txt"
  |> List.map ~f:Port.of_string
  |> Port.Set.of_list

let bridge_strength bridge =
  List.fold bridge ~init:0 ~f:(fun acc port -> acc + Port.strength port)

let find_bridges ports =
  let rec aux ports need result =
    let connectable = Port.Set.filter ports ~f:(Port.can_connect need) in
    if Port.Set.is_empty connectable then [result]
    else
      Set.fold connectable ~init:[] ~f:(fun acc port ->
          let available = Port.Set.remove ports port in
          let need = Port.opposite need port in
          List.append (aux available need (port::result)) acc
        )
  in aux ports 0 []

let _ =
  let available_ports = parse_input () in
  let bridges = find_bridges available_ports in

  List.fold bridges ~init:0 ~f:(fun m bridge -> Int.max m (bridge_strength bridge))
  |> printf "max bridge strength: %d\n";

  List.max_elt bridges ~cmp:(fun a b -> Int.compare (List.length a) (List.length b))
  |> Option.value ~default:[]
  |> bridge_strength
  |> printf "longest bridge strength: %d\n";