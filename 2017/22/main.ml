open Core

type state = { position:Point.t; direction:Direction.t; infected:int; }

let health map p =
  Point.Map.find map p |> Option.value ~default:Health.Clean

let will_infect map point =
  match health map point with
  | Health.Weakened -> true
  | _ -> false

let turn map state =
  let response t dir =
    match t with
    | Health.Clean -> Direction.turn_left dir
    | Health.Weakened -> dir
    | Health.Infected -> Direction.turn_right dir
    | Health.Flagged -> Direction.reverse dir in
  let health = health map state.position in
  response health state.direction

let toggle map point =
  let f health =
    health
    |> Option.value ~default:Health.Clean
    |> Health.next_state
  in Point.Map.update map point ~f

let rec solve map state n =
  if n = 0 then state
  else
    let direction = turn map state in
    let position = Direction.go direction state.position in
    let k = if will_infect map state.position then 1 else 0 in
    let new_map = toggle map state.position in
    solve new_map { position; direction; infected=state.infected + k; } (n-1)

let parse_input () =
  let lines = In_channel.read_lines "./input.txt" in
  let n = List.length lines in
  let k = -n / 2 in
  let parse_line y map line =
    let chars = String.to_list line in
    let parse_chars x map c =
      let key = (x + k, -k - y)
      and data = Health.of_char c
      in Point.Map.add map ~key ~data
    in List.foldi chars ~init:map ~f:parse_chars
  in List.foldi lines ~init:Point.Map.empty ~f:parse_line

let _ =
  let map = parse_input () in
  let result = solve map { position=(0, 0); direction=Direction.Up; infected=0; } 10000000 in
  printf "b: %d" result.infected