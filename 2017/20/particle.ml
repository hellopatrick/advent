open Core

type t = { mutable p:Vector.t; mutable v:Vector.t; a:Vector.t; }
[@@deriving sexp, compare]

let compare_manhattan a b =
  Vector.compare_manhattan a.p b.p

let collide a b =
  Vector.same a.p b.p

let to_string t =
  Vector.to_string t.p

let step t =
  let v = Vector.add t.v t.a in
  let p = Vector.add t.p v in
  t.p <- p;
  t.v <- v;