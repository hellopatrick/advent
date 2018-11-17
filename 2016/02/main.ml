open Core 

type axis = Vertical | Horizontal

let inspect fmt z = 
  printf fmt z; z

let print_pair (a,b) = 
  printf "[%d,%d] " a b

let keep a b =
  match a with 
  | 0 -> if b < 0 then (max (-2) b) else (min 2 b)
  | 1 | -1 -> if b < 0 then (max (-1) b) else (min 1 b)
  | _ -> 0

let clamp (x,y) dt = 
  match dt with 
  | Vertical -> (x , keep x y)
  | Horizontal -> (keep y x, y)

let traverse acc steps = 
  let follow (x,y) step = 
    match step with 
    | 'U' -> clamp (x, (y+1)) Vertical
    | 'L' -> clamp ((x-1), y) Horizontal
    | 'R' -> clamp ((x+1), y) Horizontal
    | 'D' -> clamp (x, (y-1)) Vertical
    | _ -> raise (Invalid_argument "uhh")
  in
  let first = List.hd_exn acc in
  let rest = List.fold ~init:first ~f:follow steps in 
  rest::acc

let _ =
  In_channel.read_lines "./input.txt"
  |> List.map ~f:String.to_list
  |> List.fold ~init:[(-2,0)] ~f:traverse
  |> List.iter ~f:print_pair