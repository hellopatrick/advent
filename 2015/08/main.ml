open Core

let encoded_length str =
  let rec aux chars l =
    match chars with
    | [] -> List.length l + 2
    | '\\'::tl -> aux tl ('\\'::'\\'::l)
    | '"'::tl -> aux tl ('\\'::'"'::l)
    | c::tl -> aux tl (c::l)
  in aux (String.to_list str) []

let decoded_length str =
  let rec aux chars n =
    match chars with
    | [] -> (n - 2)
    | '\\'::'\\'::tl -> aux tl (n+1)
    | '\\'::'"'::tl -> aux tl (n+1)
    | '\\'::'x'::_::_::tl -> aux tl (n+1)
    | _::tl -> aux tl (n+1)
  in aux (String.to_list str) 0

let _ =
  let lines = In_channel.read_lines "./input.txt" in
  let cc = List.map lines ~f:(fun s -> (String.length s) - (decoded_length s)) in
  List.fold cc ~init:0 ~f:Int.(+)
  |> printf "a: %d\n";

  let cc = List.map lines ~f:(fun s -> (encoded_length s) - (String.length s)) in
  List.fold cc ~init:0 ~f:Int.(+)
  |> printf "b: %d\n";