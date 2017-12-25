open Core

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

let happiness feelings arrangement =
  let arrangement = List.to_array arrangement in
  let n = Array.length arrangement in
  let f i p =
    let h = if i = 0 then n - 1 else i - 1
    and k = if i = n - 1 then 0 else i + 1 in
    let o = arrangement.(h)
    and q = arrangement.(k) in
    Pair.Map.find_exn feelings (p, o) + Pair.Map.find_exn feelings (p, q)
  in Array.mapi arrangement ~f
     |> Array.fold ~init:0 ~f:(Int.(+))

let parse_input () =
  In_channel.read_lines "./input.txt"
  |> List.filter_map ~f:Pair.of_string
  |> Pair.Map.of_alist_exn

let get_people graph =
  Pair.Map.keys graph
  |> List.fold ~init:String.Set.empty ~f:(fun set (p, _) -> Set.add set p)

let add_me graph people =
  let f acc person =
    Pair.Map.add acc ~key:("me", person) ~data:0
    |> Pair.Map.add ~key:(person, "me") ~data:0
  in
  let graph = Set.fold people ~init:graph ~f
  and people = Set.add people "me" in
  graph, people

let _ =
  let graph = parse_input () in
  let people = get_people graph in
  let arrangements = permutations people in
  Sequence.map arrangements ~f:(happiness graph)
  |> Sequence.max_elt ~cmp:Int.compare
  |> Option.value ~default:0
  |> printf "best happiness: %d\n";

  let graph = parse_input () in
  let people = get_people graph in
  let graph, people = add_me graph people in
  let arrangements = permutations people in
  Sequence.map arrangements ~f:(happiness graph)
  |> Sequence.max_elt ~cmp:Int.compare
  |> Option.value ~default:0
  |> printf "best happiness with me: %d";