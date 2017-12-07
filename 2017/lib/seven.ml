open Core
open Re2


let rec get_weight tree weights node =
    let my_weight = String.Map.find_exn weights node in
    let children = String.Map.find_exn tree node in
    let childrens_weight = List.map children ~f:(get_weight tree weights) in
    my_weight + List.fold childrens_weight ~init:0 ~f:Int.(+)

let unbalanced_child children tree weights =
    if List.is_empty children then None else
    let children_weights = List.map children ~f:(get_weight tree weights) in
    let children_weight_set = Int.Set.of_list children_weights in
    if Set.length children_weight_set = 1 then None else
    let min = Int.Set.min_elt_exn children_weight_set in
    let max = Int.Set.max_elt_exn children_weight_set in
    let min_count = List.count children_weights ~f:(fun w -> w = min) in
    let max_count = List.count children_weights ~f:(fun w -> w = max) in
    let off, target = (if min_count < max_count then (min, max) else (max, min)) in
    let zip = (List.zip_exn children children_weights) in
    let off_child, _ = List.find_exn zip ~f:(fun (a, b) -> b = off) in
    Some (off_child, target)

let rec balance root tree weights target =
    let children = String.Map.find_exn tree root in
    let children_weights = List.map children ~f:(get_weight tree weights) in
    let total_child_weight = List.fold children_weights ~init:0 ~f:Int.(+) in
    match unbalanced_child children tree weights with
    | None -> Int.abs (total_child_weight - target)
    | Some (child, desired) -> balance child tree weights desired

let get_nodes nodes =
    let init = String.Set.empty in
    let f set (h, _, _) = String.Set.add set h in
    List.fold nodes ~init ~f

let node_to_weights nodes =
    let init = String.Map.empty in
    let f map (key, data, _) = String.Map.add map ~key ~data in
    List.fold nodes ~init ~f

let parent_to_children nodes =
    let init = String.Map.empty in
    let f map (key, _, data) = String.Map.add map ~key ~data in
    List.fold nodes ~init ~f

let get_child_nodes nodes =
    let init = String.Set.empty in
    let f set (_, _, c) = String.Set.union set (String.Set.of_list c) in
    List.fold nodes ~init ~f

let parse_children children =
    String.split children ~on:',' |> List.map ~f:String.strip

let handle_match matches =
    let parent = matches.(1) in
    let weight = matches.(2) in
    let children = matches.(3) in
    match parent, weight, children with
    | Some parent, Some weight, Some children -> Some (parent, (Int.of_string weight), (parse_children children))
    | Some parent, Some weight, None -> Some (parent, (Int.of_string weight), [])
    | _, _, _ -> None

let handle_line r line =
    match Re2.Regex.find_submatches r line with
    | Ok matches -> handle_match matches
    | Error _ -> None

let parse_input () =
    let lines = In_channel.read_lines "./2017/data/7.txt" in
    let r = Regex.create_exn "([a-z]+) \\(([0-9]+)\\)(?: -> ([\\w, ]*)+)?" in
    List.filter_map lines ~f:(handle_line r)

let solve () =
    let input = parse_input () in
    let nodes = get_nodes input in
    let children = get_child_nodes input in
    let root = String.Set.choose_exn (String.Set.diff nodes children) in
    printf "a: %s\n" root;
    let weight_map = node_to_weights input in
    let tree = parent_to_children input in
    let new_weight = balance root tree weight_map 0 in
    printf "b: %d\n" new_weight;