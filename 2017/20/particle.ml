open Core

type t = { mutable p:Vector.t; mutable v:Vector.t; a:Vector.t; }

let compare_manhattan a b =
  Vector.compare_manhattan a.p b.p

let collide a b =
  Vector.equal a.p b.p

let to_string t =
  let p = Vector.to_string t.p
  and v = Vector.to_string t.v
  and a = Vector.to_string t.a
  in sprintf "p=%s; v=%s; a=%s;" p v a

let step t =
  let v = Vector.add t.v t.a in
  let p = Vector.add t.p v in
  t.p <- p;
  t.v <- v;