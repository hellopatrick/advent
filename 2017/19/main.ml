open Core

module Point = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)
end

type direction = Down | Up | Left | Right
type tube = Tube | Letter of char
type state = { dir:direction; current:Point.t; steps:int; letters:char list; }

let next_point (x, y) = function
  | Down -> (x, y+1)
  | Up -> (x, y-1)
  | Left -> (x-1, y)
  | Right -> (x+1, y)

let turn = function
  | Up | Down -> [Left; Right]
  | Left | Right -> [Up; Down]

let check_turn map (x, y) dir =
  let turns = turn dir in
  match List.find turns ~f:(fun turn ->
      let next = next_point (x,y) turn in Point.Map.find map next |> Option.is_some
    ) with
  | None -> None
  | Some t -> Some ((next_point (x,y) t), t)

let next_move map state =
  let next = next_point state.current state.dir in
  match Point.Map.find map next with
  | None -> check_turn map state.current state.dir
  | Some _ -> Some (next, state.dir)

let check_for_letter map point letters =
  match Point.Map.find_exn map point with
  | Tube -> letters
  | Letter c -> c::letters

let solve map =
  let start map =
    Point.Map.keys map
    |> List.find ~f:(fun (x,y) -> y = 0)
    |> Option.value ~default:(0, 0)
  in
  let rec aux map state =
    let letters = check_for_letter map state.current state.letters in
    let steps = state.steps + 1 in
    match next_move map state with
    | None -> {state with letters; steps}
    | Some (current, dir) -> aux map {dir; current; letters; steps}
  in
  let state = { dir=Down; current=(start map); letters=[]; steps=0 } in
  aux map state

let parse_input () =
  let lines = In_channel.read_lines "./input.txt" in
  List.foldi lines ~init:(Point.Map.empty) ~f:(fun y map line ->
      List.foldi (String.to_list line) ~init:map ~f:(fun x map char ->
          match char with
          | ' ' -> map
          | ('A'..'Z' as c) -> Point.Map.add map ~key:(x, y) ~data:(Letter c)
          | _ -> Point.Map.add map ~key:(x, y) ~data:Tube
        )
    )

let _ =
  let map = parse_input () in
  let state = solve map in
  let visited = List.rev state.letters |> String.of_char_list in
  printf "%s -> %d\n" visited state.steps
