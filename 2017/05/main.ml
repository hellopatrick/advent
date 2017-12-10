open Core

let rec run_a instructions current n =
  if current < 0 || current >= Array.length instructions then n
  else
    let jump = instructions.(current) in
    instructions.(current) <- (jump + 1);
    run_a instructions (jump + current) (n + 1)

let rec run_b instructions current n =
  if current < 0 || current >= Array.length instructions then n
  else
    let jump = instructions.(current) in
    let increment = if jump >= 3 then -1 else 1 in
    instructions.(current) <- (jump + increment);
    run_b instructions (jump + current) (n + 1)

let parse_input () =
  In_channel.read_lines "./moves.txt"
  |> List.map ~f:Int.of_string
  |> Array.of_list

let _ =
  let instructions = parse_input () in
  let result = run_a instructions 0 0 in
  printf "a: %d\n" result;

  let instructions = parse_input () in
  let result = run_b instructions 0 0 in
  printf "b: %d\n" result;