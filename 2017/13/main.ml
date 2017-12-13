open Core

module Layer = struct
  type t = { n:int; depth:int; }

  let will_catch layer delay =
    (layer.n + delay) % (2 * layer.depth - 2) = 0

  let severity layer = layer.n * layer.depth
end

let is_caught layers delay =
  let f layer = Layer.will_catch layer delay in
  List.find layers ~f 
  |> Option.is_some

let severity_of_traversal layers delay =
  let f layer = Layer.will_catch layer delay in
  List.filter layers ~f
  |> List.map ~f:Layer.severity
  |> List.fold ~init:0 ~f:Int.(+)

let find_safe_time layers =
  let rec aux layers n =
    if is_caught layers n then aux layers (n + 1)
    else n
  in aux layers 0

let parse_inputs () =
  let parse_line line =
    let to_layer = function
      | [n; depth] -> Some Layer.{n; depth;}
      | _ -> None
    in
    String.split line ~on:':'
    |> List.map ~f:(Fn.compose Int.of_string String.strip)
    |> to_layer
  in
  let open Layer in
  In_channel.read_lines "./input.txt"
  |> List.filter_map ~f:parse_line

let _ =
  let input = parse_inputs () in
  let severity = severity_of_traversal input 0 in
  printf "a: %d\n" severity;
  let brute = find_safe_time input in
  printf "b: %d\n" brute;