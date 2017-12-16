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
  let rotate a =
    let v = a.(0) in
    for i = 0 to len-2 do
      a.(i) <- a.(i+1)
    done;
    a.(len-1) <- v
  in
  let rec aux = function
    | 0 -> ()
    | n -> (rotate array); aux (n-1)
  in aux (len - n)

let exchange = Array.swap

let partner array a b =
  let j, _ = Array.findi_exn array ~f:(fun _ c -> Char.equal a c)
  and k, _ = Array.findi_exn array ~f:(fun _ c -> Char.equal b c) in
  exchange array j k

let perform array = function
  | Spin n -> spin array n; array
  | Exchange (j, k) -> exchange array j k; array
  | Partner (a, b) -> partner array a b; array
