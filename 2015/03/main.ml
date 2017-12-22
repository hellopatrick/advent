open Core

type direction = Up | Left | Right | Down

module Point = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)

  let go (x, y) = function
  | Up -> (x, y+1)
  | Down -> (x, y-1)
  | Left -> (x-1, y)
  | Right -> (x+1, y)
end

type state = { visited:Point.Set.t; santa_position: Point.t; robo_position: Point.t }

let fly_santa_fly directions =
  let init = { visited = Point.Set.of_list [(0,0)]; santa_position=(0,0); robo_position=(0,0) } in
  let fly i state direction =
    if i % 2 = 0 then
      let santa_position = Point.go state.santa_position direction in
      let visited = Point.Set.add state.visited santa_position in
      {state with santa_position; visited}
    else
      let robo_position = Point.go state.robo_position direction in
      let visited = Point.Set.add state.visited robo_position in
      {state with robo_position; visited}
  in
  List.foldi directions ~init ~f:fly

let parse_input () =
  let to_direction = function
    | '<' -> Some Left
    | '>' -> Some Right
    | '^' -> Some Up
    | 'v' -> Some Down
    | _ -> None
  in
  In_channel.read_all "./input.txt"
  |> String.to_list
  |> List.filter_map ~f:to_direction

let _ =
  let directions = parse_input () in
  let state = fly_santa_fly directions in
  let houses_visited = Set.length state.visited in
  printf "houses: %d\n" houses_visited