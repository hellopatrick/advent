open Core

let range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux (j - 1) []

module Coords = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end

module Claim = struct
  type t = {id: int; x: int; y: int; width: int; height: int}

  let of_string s =
    let id, x, y, width, height =
      Scanf.sscanf s "#%d @ %d,%d: %dx%d" (fun id x y width height ->
          (id, x, y, width, height) )
    in
    {id; x; y; width; height}

  let coords t =
    let xs = range t.x (t.x + t.width) in
    let ys = range t.y (t.y + t.height) in
    List.cartesian_product xs ys

  let stake all_claims t =
    let f = Option.value_map ~default:1 ~f:(fun v -> v + 1) in
    let coords = coords t in
    List.fold coords ~init:all_claims ~f:(fun claims coord ->
        Coords.Map.update claims coord ~f )

  let intact all_claims t =
    let f coord =
      match Coords.Map.find all_claims coord with
      | None -> false
      | Some i -> i = 1
    in
    List.for_all ~f (coords t)
end

let input () =
  In_channel.read_lines "./input.txt" |> List.map ~f:Claim.of_string

let build_claim_map input =
  List.fold input ~init:Coords.Map.empty ~f:Claim.stake

let part_one claim_map = Coords.Map.count ~f:(fun v -> v > 1) claim_map

let part_two claims claim_map =
  List.find ~f:(Claim.intact claim_map) claims
  |> Option.value_map ~default:(-1) ~f:(fun (claim : Claim.t) -> claim.id)

let _ =
  let claims = input () in
  let claim_map = build_claim_map claims in
  printf "overlapping claims: %d" (part_one claim_map) ;
  printf "\n" ;
  printf "safe claim: %d" (part_two claims claim_map)
