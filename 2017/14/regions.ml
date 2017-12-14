open Core

module Point = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)
end

type t = { n:Point.t; links:Point.t list}

let rec travel map visited n =
  if Point.Set.mem visited n then visited
  else
    let visited = Point.Set.add visited n in
    let travel' = travel map in
    let children = Point.Map.find_exn map n in
    let f visited child = Point.Set.union (travel' visited child) visited in
    List.fold children ~init:visited ~f

let groups set map =
  let rec aux unvisited map groups =
    match Point.Set.choose unvisited with
    | None -> groups
    | Some r ->
      let new_group = travel map (Point.Set.empty) r in
      aux (Set.diff unvisited new_group) map (new_group::groups)
  in aux set map []

let count_regions pipes =
  let create_map acc pipe = Point.Map.add acc ~key:pipe.n ~data:pipe.links in
  let pipe_map = List.fold pipes ~init:Point.Map.empty ~f:create_map in

  let create_set acc pipe = Point.Set.add acc pipe.n in
  let unvisited = List.fold pipes ~init:Point.Set.empty ~f:create_set in
  let groups = groups unvisited pipe_map in
  List.length groups