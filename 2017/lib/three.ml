open Core

(* Part A of Day 3 was done by hand... *)

module Direction = struct
    type t = Left | Right | Up | Down

    let increment = function
    | Right -> (1, 0)
    | Left -> (-1, 0)
    | Up -> (0, 1)
    | Down -> (0, -1)

    let leftside = function
    | Right -> Up
    | Left -> Down
    | Up -> Left
    | Down -> Right
end

module Point = struct
    type t = int * int

    let compare (x0, y0) (x1, y1) =
        match Int.compare x0 x1 with
        | 0 -> Int.compare y0 y1
        | r -> r

    let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
    let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple

    let move (x, y) dir =
        match dir with
        | Direction.Right -> (x+1, y)
        | Direction.Left -> (x-1, y)
        | Direction.Up -> (x, y+1)
        | Direction.Down -> (x, y-1)
end

module PointMap = Map.Make(Point)

let neighbors (x, y) =
    [
        (x-1, y+1); (x, y+1); (x+1, y+1);
        (x-1, y); (x+1, y);
        (x-1, y-1); (x, y-1); (x+1, y-1);
    ]

let find map k =
    match Map.find map k with
    | Some n -> n
    | None -> 0

let neighbor_values map (x, y) =
    let neighbors = neighbors (x, y) in
    List.map neighbors ~f:(find map)

let sum_neighbors map point =
    neighbor_values map point |> List.fold ~init:0 ~f:Int.(+)

let next_dir map point dir =
    let left = Direction.leftside dir in
    let next = Point.move point left in
    match Map.find map next with
    | Some _ -> dir
    | None -> left

let rec spiral map prev dir goal =
    let curr = Point.move prev dir in
    let n = sum_neighbors map curr in
    let map = Map.add ~key:curr ~data:n map in
    if n > goal then n
    else
        let next_dir = next_dir map curr dir in
        spiral map curr next_dir goal

let solve () =
    let m = PointMap.of_alist_exn [(0, 0), 1] in
    let j = spiral m (0, 0) Direction.Right 368078 in
    printf "b: %d\n" j