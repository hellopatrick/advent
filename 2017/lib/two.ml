open Core

let rec divisible numbers =
    let check a b =
        if a % b = 0 then Some (a / b)
        else if b % a = 0 then Some (b / a)
        else None
    in
    match numbers with
    | [] -> 0
    | h::t ->
        match List.find_map t ~f:(check h) with
        | Some n -> n
        | None -> divisible t

let spread numbers =
    let init = Int.max_value, Int.min_value
    and f (min, max) n = (Int.min min n, Int.max max n) in
    let min, max = List.fold ~init ~f numbers in
    max - min

let parse line =
    String.split line ~on:'\t'
    |> List.map ~f:Int.of_string

let sum = List.fold ~init:0 ~f:(fun acc n -> acc + n)

let solve =
    let values = In_channel.read_lines "./2017/data/2.txt" |> List.map ~f:parse in
    let a = List.map values ~f:spread |> sum in
    let b = List.map values ~f:divisible |> sum in
    printf "a: %d\n" a;
    printf "b: %d\n" b;
