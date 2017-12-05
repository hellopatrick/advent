open Core

let rec run_a instructions current n =
    if current >= Array.length instructions then n
    else
        let jump = Array.get instructions current in
        Array.set instructions current (jump + 1);
        run_a instructions (jump + current) (n + 1)

let rec run_b instructions current n =
    if current >= Array.length instructions then n
    else
        let jump = Array.get instructions current in
        let increment = if jump >= 3 then -1 else 1 in
        Array.set instructions current (jump + increment);
        run_b instructions (jump + current) (n + 1)

let parse_input () =
    In_channel.read_lines "./2017/data/5.txt"
    |> List.map ~f:Int.of_string
    |> Array.of_list

let solve () =
    let instructions = parse_input () in
    let result = run_a instructions 0 0 in
    printf "a: %d\n" result;

    let instructions = parse_input () in
    let result = run_b instructions 0 0 in
    printf "b: %d\n" result;