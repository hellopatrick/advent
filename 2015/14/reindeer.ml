open Core

type t = { name:string; speed:int; time_to_fly:int; time_to_rest:int; }

let of_string str =
  match String.split str ~on:' ' with
  | [name; _; _; speed; _; _; time_to_fly; _; _; _; _; _; _; time_to_rest;  _;] ->
    let speed = Int.of_string speed
    and time_to_fly = Int.of_string time_to_fly
    and time_to_rest = Int.of_string time_to_rest in
    Some {name; speed; time_to_fly; time_to_rest}
  | _ -> None