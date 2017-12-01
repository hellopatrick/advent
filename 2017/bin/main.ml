open Core
open Advent

let _ =
    let sum = One.a "./2017/data/1a.txt" in
    printf "a: %d\n" sum;

    let sum = One.b "./2017/data/1a.txt" in
    printf "b: %d\n" sum