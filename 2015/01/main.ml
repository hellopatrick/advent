open Core

let step i = function
  | '(' -> i + 1
  | ')' -> i - 1
  | _ -> 0

let part_a instructions =
  let f acc = step acc
  in List.fold instructions ~init:0 ~f

let part_b instructions =
  let rec solve list n f =
    if f < 0 then n else
      match list with
      | [] -> -1
      | hd::tl -> solve tl (n+1) (step f hd)
  in solve instructions 0 0

let parse_input () =
  In_channel.read_all "./input.txt"
  |> String.to_list

let _ =
  let instructions = parse_input () in
  part_a instructions |> printf "a: %d\n";
  part_b instructions |> printf "b: %d\n";