open Core

type v = Value of int | Register of char

type t =
  | Send of char
  | Set of (char * v)
  | Multiply of (char * v)
  | Add of (char * v)
  | Modulus of (char * v)
  | Receive of char
  | Jump of (v * v)