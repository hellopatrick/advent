open Core

module Edge = struct
  include Tuple.Make (String) (String)
  include Tuple.Comparable (String) (String)
end

let permutations lst =
  let lstar = Set.to_array lst in
  let len = Array.length lstar in
  let ks = List.range 1 (len + 1) in
  let indices = Int.Set.of_list (List.range 0 len) in
  let choose k (v, indices, res) =
    let ix = Option.value_exn (Int.Set.nth indices (v mod k)) in
    (v / k, Int.Set.remove indices ix, lstar.(ix) :: res)
  in
  let permute i =
    let (v, _, res) =
      List.fold_right ks ~f:choose ~init:(i, indices, [])
    in
    if v > 0 then None else Some (res, i+1)
  in
  Sequence.unfold ~init:0 ~f:permute

let to_edge str =
  match String.split str ~on:' ' with
  | [start; "to"; dest; "="; dist] -> (start, dest, Int.of_string dist)
  | _ -> failwith "incorrect line"

let distance_of_route graph route =
  let f acc edge =
    acc + Map.find_exn graph edge
  in
  let to_edges route =
    let rec aux route edges =
      match route with
      | [_] | [] -> edges
      | start::dest::rest -> aux (dest::rest) ((start,dest)::edges)
    in aux route []
  in to_edges route |> List.fold ~init:0 ~f

let parse_input () =
  let into_map m (start, dest, data) =
    let m = Map.add m ~key:(start, dest) ~data in
    Map.add m ~key:(dest, start) ~data
  in
  let lines = In_channel.read_lines "./input.txt" in
  let graph = List.map lines ~f:to_edge |> List.fold ~init:Edge.Map.empty ~f:into_map
  and places = List.map lines ~f:to_edge |> List.concat_map ~f:(fun (s, d, _) -> [s; d;]) |> String.Set.of_list in
  graph, places

let _ =
  let graph, cities = parse_input () in
  let routes = permutations cities in
  let minimum = Sequence.fold routes ~init:Int.max_value ~f:(fun min_dist route -> Int.min min_dist (distance_of_route graph route) ) in
  printf "min: %d\n" minimum;

  let routes = permutations cities in
  let maximum = Sequence.fold routes ~init:0 ~f:(fun max_dist route -> Int.max max_dist (distance_of_route graph route) ) in
  printf "max: %d\n" maximum