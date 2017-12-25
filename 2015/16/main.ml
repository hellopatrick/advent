open Core

(*  *)

let parse_input filename =
  let f channel =
    let parse lexbuf = Parser.aunts Lexer.read lexbuf in
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let is_equal_or_missing value required =
  value = required || value = -1

let is_greater_than_or_missing value required =
  value > required || value = -1

let is_less_than_or_missing value required =
  value < required || value = -1

let _ =
  let aunts =
    parse_input "./input.txt"
    |> List.mapi ~f:(fun i aunt -> {aunt with name=(i+1);}) in
  let aunt =
    List.filter aunts ~f:(fun aunt -> is_equal_or_missing aunt.children 3)
    |> List.filter ~f:(fun aunt -> is_greater_than_or_missing aunt.cats 7)
    |> List.filter ~f:(fun aunt -> is_equal_or_missing aunt.samoyeds 2)
    |> List.filter ~f:(fun aunt -> is_less_than_or_missing aunt.pomeranians 3)
    |> List.filter ~f:(fun aunt -> is_equal_or_missing aunt.akitas 0)
    |> List.filter ~f:(fun aunt -> is_equal_or_missing aunt.vizslas 0)
    |> List.filter ~f:(fun aunt -> is_less_than_or_missing aunt.goldfish 5)
    |> List.filter ~f:(fun aunt -> is_greater_than_or_missing aunt.trees 3)
    |> List.filter ~f:(fun aunt -> is_equal_or_missing aunt.cars 2)
    |> List.filter ~f:(fun aunt -> is_equal_or_missing aunt.perfumes 1)
    |> List.hd in
  match aunt with
  | Some aunt -> printf "Sue %d sent it!\n" aunt.name
  | None -> ()
