open Core

type range = { top_left:(int*int); bottom_right:(int*int) }

type t = On of range | Off of range | Toggle of range

let iter array range f =
  let act py px s =
    let x, y = range.top_left
    and x', y' = range.bottom_right in
    if x <= px && px <= x' && y <= py && py <= y' then array.(py).(px) <- f s
  in
  let iter_row y row = Array.iteri row ~f:(act y) in
  Array.iteri array ~f:iter_row

let turn_on array range =
  let f s = s + 1 in
  iter array range f

let turn_off array range =
  let f s = Int.max (s-1) 0 in
  iter array range f

let toggle array range =
  let f s = s + 2 in
  iter array range f

let follow ts array =
  let f = function
    | On r -> turn_on array r
    | Off r -> turn_off array r
    | Toggle r -> toggle array r
  in
  List.iter ts ~f