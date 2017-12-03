open Core

module Direction = struct
    type t = Left | Right | Up | Down

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

    let neighbors (x, y) =
        [
            (x-1, y+1); (x, y+1); (x+1, y+1);
            (x-1, y); (x+1, y);
            (x-1, y-1); (x, y-1); (x+1, y-1);
        ]
end

module PointMap = struct
    include Map.Make(Point)

    let find_or map k ~default =
        find map k |> Option.value ~default

    let neighbors map (x, y) =
        let neighbors = Point.neighbors (x, y) in
        List.map neighbors ~f:(find_or map ~default:0)
end

let sum_neighbors map point =
    PointMap.neighbors map point |> List.fold ~init:0 ~f:Int.(+)

let next_dir map point dir =
    let left = Direction.leftside dir in
    let next = Point.move point left in
    match Map.find map next with
    | Some _ -> dir
    | None -> left

let rec a_spiral map prev dir goal =
    let curr = Point.move prev dir in
    let n = PointMap.find map prev |> Option.value_map ~default:0 ~f:(fun prev -> prev + 1) in
    let map = PointMap.add ~key:curr ~data:n map in
    if n = goal then curr
    else
        let next_dir = next_dir map curr dir in
        a_spiral map curr next_dir goal

let rec b_spiral map prev dir goal =
    let curr = Point.move prev dir in
    let n = sum_neighbors map curr in
    let map = PointMap.add ~key:curr ~data:n map in
    if n > goal then n
    else
        let next_dir = next_dir map curr dir in
        b_spiral map curr next_dir goal

let solve () =
    let m = PointMap.of_alist_exn [(0, 0), 1] in

    let x, y = a_spiral m (0, 0) Direction.Right 368078 in
    printf "a: %d\n" Int.(abs x + abs y);

    let j = b_spiral m (0, 0) Direction.Right 368078 in
    printf "b: %d\n" j;