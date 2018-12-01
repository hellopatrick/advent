open Core

let input = In_channel.read_lines "input.txt" |> List.map ~f:Int.of_string
let part_one = input |> List.reduce ~f:( + ) |> Option.value ~default:0

let running_sum acc curr =
  let open Container.Continue_or_stop in
  let seen, value = acc in
  let next = value + curr in
  if Set.mem seen next then Stop next else Continue (Set.add seen next, next)

let finish _ = -1

let part_two =
  let init = (Int.Set.empty, 0) in
  input |> Sequence.cycle_list_exn
  |> Sequence.fold_until ~init ~f:running_sum ~finish

let _ =
  printf "%d" part_one ; Out_channel.newline stdout ; printf "%d" part_two
