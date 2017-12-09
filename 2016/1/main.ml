open Core

module Point = struct
    include Tuple.Make (Int) (Int)
    include Tuple.Comparable (Int) (Int)
    include Tuple.Hashable (Int) (Int)
end

type move = Left of int | Right of int
type direction = North | South | East | West
type visited_map = bool Point.Map.t

type state = {current:Point.t; facing:direction; visited:visited_map;}

let steps = function
| Left n | Right n -> n

let rotate = function
| North -> (function | Left _ -> West | Right _ -> East)
| South -> (function | Left _ -> East | Right _ -> West)
| West -> (function | Left _ -> South | Right _ -> North)
| East -> (function | Left _ -> North | Right _ -> South)

let check_visit visited (x, y) =
    match Point.Map.find visited (x, y) with
    | Some true -> printf "previously visited: (%d, %d) (%d)\n" x y Int.(abs x + abs y)
    | _ -> ()

let rec go state n =
    if n = 0 then state else
    let x, y = state.current in
    let current = match state.facing with
    | North -> x, y + 1
    | South -> x, y - 1
    | East -> x + 1, y
    | West -> x - 1, y in
    check_visit state.visited current;
    let visited = Map.add state.visited ~key:current ~data:true in
    go {state with current; visited;} (n-1)

let follow state move =
    let facing = rotate state.facing move and steps = steps move in
    go {state with facing} steps

let to_move str =
    let direction = str.[0] in
    let count = String.drop_prefix str 1 |> Int.of_string in
    match direction with
    | 'R' -> Some (Right count)
    | 'L' -> Some (Left count)
    | _ -> None

let input () =
    In_channel.read_all "./directions.txt"
    |> String.split_on_chars ~on:[',']
    |> List.filter_map ~f:(Fn.compose to_move String.strip)

let _ =
    let moves = input () in
    let init = {
        current=(0,0);
        facing=North;
        visited=Point.Map.of_alist_exn [(0,0), true;];
    } in
    let state = List.fold moves ~init ~f:follow in
    let x, y = state.current in
    printf "%d\n" Int.(abs x + abs y)