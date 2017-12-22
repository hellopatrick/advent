type t = Left | Right | Up | Down

let turn_left = function
  | Right -> Up
  | Left -> Down
  | Up -> Left
  | Down -> Right

let turn_right = function
  | Right -> Down
  | Left -> Up
  | Up -> Right
  | Down -> Left

let reverse = function
  | Right -> Left
  | Left -> Right
  | Up -> Down
  | Down -> Up

let go t (x, y) =
  match t with
  | Up -> (x, y+1)
  | Down -> (x, y-1)
  | Right -> (x+1, y)
  | Left -> (x-1, y)