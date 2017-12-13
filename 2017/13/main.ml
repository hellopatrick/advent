open Core

module Layer = struct
  type t = { n:int; depth:int; }

  let of_string str =
    let convert = function
      | [n; depth] -> Some {n; depth;}
      | _ -> None
    in
    String.split str ~on:':'
    |> List.map ~f:(Fn.compose Int.of_string String.strip)
    |> convert

  let will_catch ?(delay=0) layer =
    (layer.n + delay) % (2 * layer.depth - 2) = 0

  let severity layer =
    layer.n * layer.depth
end

let is_caught ?(delay=0) layers =
  List.find layers ~f:(Layer.will_catch ~delay)
  |> Option.is_some

let severity_of_traversal ?(delay=0) layers =
  List.filter layers ~f:(Layer.will_catch ~delay)
  |> List.map ~f:Layer.severity
  |> List.fold ~init:0 ~f:Int.(+)

let find_safe_time layers =
  let rec aux layers delay =
    if is_caught layers ~delay then aux layers (delay + 1)
    else delay
  in aux layers 0

let parse_inputs () =
  In_channel.read_lines "./input.txt"
  |> List.filter_map ~f:Layer.of_string

let _ =
  let input = parse_inputs () in
  let severity = severity_of_traversal input in
  printf "a: %d\n" severity;
  let brute = find_safe_time input in
  printf "b: %d\n" brute;