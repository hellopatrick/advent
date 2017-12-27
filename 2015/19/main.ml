open Core

let part_a map parts =
  let mappings s =
    match String.Map.find map s with
    | Some l -> s::l
    | None -> [s] in
  let rec aux head tail res =
    match tail with
    | [] -> res
    | s::tl ->
      let res = aux (List.append head [s]) tl res in
      let replacements = mappings s in
      let created = List.map replacements ~f:(fun r -> String.concat (List.append head (r::tl))) in
      String.Set.union (String.Set.of_list created) res
  in aux [] parts String.Set.empty

let part_b map desired terminal =
  let rec aux map curr n =
    if String.equal curr terminal then n
    else begin
      match List.find map ~f:(fun (a, substring) -> String.is_substring ~substring curr) with
      | None -> aux (List.permute map) desired 0
      | Some (with_, pattern) ->
        let next = String.substr_replace_first curr ~pattern ~with_ in
        aux map next (n+1)
    end
  in aux map desired 0

let parse_map () =
  let parse_lines line =
    match String.split line ~on:' ' with
    | [a; "=>"; b] -> Some (a, b)
    | _ -> None
  in
  In_channel.read_lines "./input.txt"
  |> List.filter_map ~f:parse_lines

let _ =
  let alist = parse_map () in
  let break a b = Char.is_uppercase b in
  let molecule = In_channel.read_all "./chemical.txt" in
  let chems = String.to_list molecule
              |> List.group ~break
              |> List.map ~f:String.of_char_list in
  let possible = part_a (String.Map.of_alist_multi alist) chems in
  let count = Set.length possible in
  printf "%d\n" (count - 1);

  let min = part_b alist molecule "e" in
  printf "%d\n" min;


