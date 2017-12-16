open Core

let process_input filename =
  let f channel =
    let parse lexbuf = Parser.moves Lexer.read lexbuf in
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let sequence init moves =
  let f init =
    let now = List.fold moves ~init ~f:Dance.perform in
    Some (now, now)
  in
  Sequence.unfold ~init ~f

let permutation_not_equal a b =
  not (Array.equal ~equal:Char.equal a b)

let dance n moves =
  let seq = sequence (String.to_array "abcdefghijklmnop") moves in
  let f = permutation_not_equal ((String.to_array "abcdefghijklmnop")) in
  let cycle = Sequence.take_while seq ~f in
  let length = Sequence.length cycle in
  let rep = n % (length + 1) in
  Sequence.nth_exn (sequence (String.to_array "abcdefghijklmnop") moves) (rep-1)

let _ =
  let moves = process_input "./input.txt" in
  dance 1_000_000_000 moves
  |> Array.to_list
  |> String.of_char_list
  |> printf "billionth -> %s\n"