open Core

let input () = In_channel.read_lines "./input.txt" 
               |> List.map ~f:String.to_list

let part_one input = 
  let count chars =
    let f map key =
      let update = function
        | None -> 1
        | Some v -> v+1 in 
      Map.update map key ~f:update
    in List.fold ~init:Char.Map.empty ~f chars 
  in 
  let char_counts = List.map ~f:count input in 
  let has count = Char.Map.exists ~f:(fun v -> v = count) in
  let twos = List.count ~f:(has 2) char_counts in 
  let threes = List.count ~f:(has 3) char_counts in 
  twos * threes

let part_two input = 
  let f (a, b) = 
    let pairs = List.zip_exn a b in 
    let diff = List.count ~f:(fun (a, b) -> a <> b) pairs
    in diff = 1 in 
  let pairs = List.cartesian_product input input 
              |> List.filter ~f:(fun (a, b) -> a <> b) 
              |> List.find ~f in 
  match pairs with 
  | Some (a, b) -> List.zip_exn a b 
                   |> List.filter_map ~f:(fun (a, b) -> if a = b then Some a else None) 
                   |> String.of_char_list
  | None -> ""

let _ = 
  printf "%d" (part_one (input ()));
  printf "\n";
  printf "%s" (part_two (input ()));