open Core

let parse_input () =
  In_channel.read_lines "./input.txt"
  |> List.map ~f:Box.of_string

let _ =
  let boxes = parse_input () in
  List.fold boxes ~init:0 ~f:(fun acc box -> acc + Box.required_wrapping_paper box)
  |> printf "a: %d\n";
  List.fold boxes ~init:0 ~f:(fun acc box -> acc + Box.required_ribbon box)
  |> printf "b: %d\n";