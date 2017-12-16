open Core

let generator_a init factor =
  let f previous =
    let m = Int.((previous * factor) % 2147483647) in
    Some (m, m)
  in
  Sequence.unfold ~init ~f

let generator_b init factor only =
  let f previous =
    let m = Int.((previous * factor) % 2147483647) in
    if m land (only - 1) = 0 then Sequence.Step.Yield (m, m)
    else Sequence.Step.Skip m
  in
  Sequence.unfold_step ~init ~f

let _ =
  let a = generator_a 703 16807
  and b = generator_a 516 48271 in
  let zipped = Sequence.zip a b in
  Sequence.take zipped 40_000_000
  |> Sequence.filter ~f:(fun (a,b) -> (a land 0xffff) = (b land 0xffff))
  |> Sequence.length
  |> printf "a: %d\n";

  Out_channel.flush stdout;

  let a = generator_b 703 16807 4
  and b = generator_b 516 48271 8 in
  let zipped = Sequence.zip a b in
  Sequence.take zipped 5_000_000
  |> Sequence.filter ~f:(fun (a,b) -> (a land 0xffff) = (b land 0xffff))
  |> Sequence.length
  |> printf "b: %d\n";