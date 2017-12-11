open Core

type direction = N | NE | NW | S | SE | SW
type hex = {x:int; y:int; z:int;}

let to_direction = function
  | "ne" -> Some NE
  | "nw" -> Some NW
  | "n" -> Some N
  | "se" -> Some SE
  | "sw" -> Some SW
  | "s" -> Some S
  | _ -> None

let move {x; y; z;} = function
  | NW -> {x=x-1; y=y+1; z}
  | SE -> {x=x+1; y=y-1; z}
  | NE -> {x=x+1; y; z=z-1}
  | SW -> {x=x-1; y; z=z+1}
  | N  -> {x; y=y+1; z=z-1}
  | S  -> {x; y=y-1; z=z+1}

let distance {x; y; z} =
  Int.max (Int.max (Int.abs x) (Int.abs y)) (Int.abs z)

let track (spot, furthest) step =
  let new_spot = move spot step in
  let distance = distance new_spot in
  if distance > furthest then (new_spot, distance)
  else (new_spot, furthest)

let read_input () =
  In_channel.read_all "./moves.txt"
  |> String.split ~on:','
  |> List.filter_map ~f:to_direction

let _ =
  let moves = read_input () in
  let current, furthest = List.fold moves ~init:({x=0; y=0; z=0;}, 0) ~f:track in
  let current_distance = distance current in
  printf "a: %d %d %d - %d\n" current.x current.y current.z current_distance;
  printf "b: %d\n" furthest