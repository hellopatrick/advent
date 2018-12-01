open Core

let input = In_channel.read_lines "input.txt"
            |> List.map ~f:Int.of_string

let part_one = 
  input
  |> List.reduce ~f:(+)
  |> Option.value ~default:0

type t = { seen:Int.Set.t; current:Int.t }

let running_sum acc curr = 
  let open Container.Continue_or_stop in
  let next = acc.current + curr in 
  if (Int.Set.mem acc.seen next) then
    Stop next
  else 
    let seen = Int.Set.add acc.seen next in 
    let current = next in 
    Continue {seen; current}

let finish t =
  t.current

let part_two = 
  let seen = Int.Set.empty in 
  let current = 0 in 
  input
  |> Sequence.cycle_list_exn
  |> Sequence.fold_until ~init:{seen; current} ~f:running_sum ~finish

let _ = 
  printf "%d\n" part_one;
  printf "%d" part_two;