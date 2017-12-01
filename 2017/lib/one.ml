open Core

let shift l n =
    let first, second = List.split_n l n in
    List.append second first

let solve n sequence =
    let f acc a b = if a = b then acc + a else acc
    in List.fold2_exn sequence (shift sequence n) ~init:0 ~f

let sequence file =
    Utils.read file
    |> String.to_list
    |> List.filter_map ~f:Char.get_digit

let a file =
    sequence file |> solve 1

let b file =
    let seq = sequence file in
    solve (List.length seq / 2) seq

let solve =
    a "./2017/data/1a.txt" |> printf "a: %d\n";
    b "./2017/data/1a.txt" |> printf "b: %d\n";