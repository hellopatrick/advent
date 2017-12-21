open Core

module T = struct
  type t = bool array array
  [@@deriving sexp, compare]
end

include Comparable.Make(T)
include T

let create n =
  Array.make_matrix ~dimx:n ~dimy:n false

let size t =
  Array.length t

let of_string s =
  String.split s ~on:'/'
  |> List.map ~f:(fun row ->
      let chars = String.to_array row in
      Array.map chars ~f:(Char.equal '#')
    )
  |> List.to_array

let sym t =
  Array.transpose_exn t

let flip t =
  let len = (Array.length t) - 1 in
  let n = (Array.length t) / 2 - 1 in
  for i = 0 to n do
    Array.swap t i (len - i)
  done;
  t

let variants t =
  [
    t;
    t |> sym;
    t |> sym |> flip;
    t |> sym |> flip |> sym;
    t |> sym |> flip |> sym |> flip;
    t |> sym |> flip |> sym |> flip |> sym;
    t |> sym |> flip |> sym |> flip |> sym |> flip;
    t |> sym |> flip |> sym |> flip |> sym |> flip |> sym;
  ]

let check_book t book =
  Map.find_exn book t