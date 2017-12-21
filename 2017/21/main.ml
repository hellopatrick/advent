open Core

let split state =
  let len = Array.length state in
  let n, step = if len % 2 = 0 then 2, len / 2 else 3, len / 3 in
  let grids = Array.init step ~f:(fun _ ->
      Array.init step ~f:(fun _ -> Grid.create n)
    ) in
  Array.iteri state ~f:(fun y row ->
      Array.iteri row ~f:(fun x value ->
          let y', dy = y / n, y % n
          and x', dx = x / n, x % n in
          grids.(y').(x').(dy).(dx) <- value
        )
    ); grids

let enhance grids book =
  let f row =
    Array.map row ~f:(fun grid -> Grid.Map.find_exn book grid)
  in Array.map grids ~f

let combine grids =
  let n = Array.length grids in
  let j = Grid.size grids.(0).(0) in
  let size = n * j in
  let state = Array.make_matrix ~dimx:size ~dimy:size true in
  Array.iteri grids ~f:(fun y row ->
      Array.iteri row ~f:(fun x grid ->
          Array.iteri grid ~f:(fun y' row' ->
              Array.iteri row' ~f:(fun x' value ->
                  state.(y * j + y').(x * j + x') <- value
                )
            )
        )
    );
  state

let parse_line line =
  match String.split line ~on:' ' with
  | [i;"=>";o] ->
    let input = Grid.of_string i in
    let output = Grid.of_string o in
    Grid.variants input, output
  | _ -> failwith "error with line."

let parse_inputs () =
  let lines = In_channel.read_lines "./input.txt" in
  List.map lines ~f:parse_line
  |> List.fold ~init:Grid.Map.empty ~f:(fun acc (inputs, output) ->
      List.fold inputs ~init:acc ~f:(fun acc grid ->
          Grid.Map.add acc ~key:grid ~data:output
        )
    )

let solve init book n =
  let rec solve' state n =
    if n = 0 then state
    else
      let grids = split state in
      let enhanced = enhance grids book in
      let state = combine enhanced in
      solve' state (n-1)
  in solve' init n

let count_on grid =
  let f acc row = acc + Array.count row ~f:Fn.id in
  Array.fold grid ~init:0 ~f

let _ =
  let book = parse_inputs () in
  let initial = [|
    [|false; true; false|];
    [|false; false; true|];
    [|true; true; true|];
  |] in
  (* let result = solve initial book 5 in
     count_on result |> printf "a: %d\n" *)
  let result = solve initial book 18 in
  count_on result |> printf "b: %d\n"
