open Core

let build key n =
  sprintf "%s-%d" key n
  |> Knot_hash.knot

let to_bit_list ary =
  let f n =
    let rec aux acc d =
      if List.length acc = 8 then acc
      else aux ((d land 1) :: acc) (d lsr 1)
    in aux [] n |> List.to_array in
  Array.concat_map ary ~f

let neighbors x y blocks =
  let f (dx, dy) =
    let get x' y' =
      if x' < 0 || x' > 127 || y' < 0 || y' > 127 then None
      else if blocks.(y').(x') = 0 then None
      else Some (x', y')
    in get (x + dx) (y + dy)
  in
  List.filter_map [(-1,0); (1,0); (0,-1); (0,1)] ~f

let to_pipe x y blocks =
  let open Regions in
  {n=(x,y); links=(neighbors x y blocks)}

let to_pipes blocks =
  Array.foldi blocks ~init:[] ~f:(fun y pipes row ->
      Array.foldi row ~init:pipes ~f:(fun x pipes i ->
          if i = 0 then pipes
          else (to_pipe x y blocks) :: pipes
        )
    )

let _ =
  let blocks = Sequence.init 128 ~f:(Fn.compose to_bit_list (build "ljoxqyyw")) in
  let used_squares = Sequence.map blocks ~f:(Array.count ~f:(Int.equal 1))
                     |> Sequence.fold ~init:0 ~f:Int.(+) in
  printf "used: %d\n" used_squares;

  let pipes = to_pipes (Sequence.to_array blocks) in
  Regions.count_regions pipes
  |> printf "regions: %d\n";