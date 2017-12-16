open Core

type t =
  | Spin of int
  | Exchange of int * int
  | Partner of char * char

let to_string = function
  | Spin n -> sprintf "spin: %d" n
  | Exchange (j, k) -> sprintf "exchange: %d %d" j k
  | Partner (a, b) -> sprintf "partner: %c %c" a b

let spin array n =
  let len = Array.length array in
  let head = Array.slice array 0 (len - n) in
  let tail = Array.slice array (len - n) len in
  Array.append tail head

let exchange = Array.swap

let partner array a b =
  let j, _ = Array.findi_exn array ~f:(fun _ c -> Char.equal a c)
  and k, _ = Array.findi_exn array ~f:(fun _ c -> Char.equal b c) in
  exchange array j k

let perform array move =
  let new_array = match move with
    | Spin n -> spin array n;
    | Exchange (j, k) -> exchange array j k; array
    | Partner (a, b) -> partner array a b; array
  in new_array
