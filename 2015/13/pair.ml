open Core

include Tuple.Make (String) (String)
include Tuple.Comparable (String) (String)

(* Alice would lose 76 happiness units by sitting next to Mallory. *)

let of_string str =
  match String.split str ~on:' ' with
  | [person; _; "gain"; happiness; _; _; _; _; _; _; other_person] ->
    let other_person = String.drop_suffix other_person 1 in
    let happiness = Int.of_string happiness in
    Some ((person, other_person), happiness)
  | [person; _; "lose"; happiness; _; _; _; _; _; _; other_person] ->
    let other_person = String.drop_suffix other_person 1 in
    let happiness = Int.of_string happiness in
    Some ((person, other_person), -happiness)
  | _ -> None