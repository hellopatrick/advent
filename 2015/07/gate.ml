open Core

type value =
  | Register of string
  | Scalar of int

type t =
  | Value of value
  | And of t * t
  | Or of t * t
  | Left of t * t
  | Right of t * t
  | Not of t

let memo = String.Table.create ()

let rec eval m t =
  match t with
  | Value (Register c) ->
    String.Table.find_or_add memo c ~default:(fun () -> eval m (String.Map.find_exn m c))
  | Value (Scalar s) -> s
  | And (a, b) -> ((eval m a) land (eval m b) land 0xFFFF)
  | Or (a, b) -> ((eval m a) lor (eval m b) land 0xFFFF)
  | Left (a, b) -> ((eval m a) lsl (eval m b) land 0xFFFF)
  | Right (a, b) -> ((eval m a) lsr (eval m b) land 0xFFFF)
  | Not (a) -> (Int.bit_not (eval m a)) land 0xFFFF