open Core

let shift l n =
    let first, second = List.split_n l n in
    List.append second first

let solve n sequence =
    let f acc a b = if a = b then acc + a else acc in
    List.fold2_exn sequence (shift sequence n) ~init:0 ~f

let sequence file =
    Utils.read file
    |> String.to_list
    |> List.filter_map ~f:Char.get_digit

let a seq =
    solve 1 seq

let b seq =
    solve (List.length seq / 2) seq

let solve =
    let seq = sequence "./2017/data/1a.txt" in
    a seq |> printf "a: %d\n";
    b seq |> printf "b: %d\n";