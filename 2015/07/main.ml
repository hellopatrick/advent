open Core

let process_input filename =
  let f channel =
    let parse lexbuf = Parser.gates Lexer.read lexbuf in
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let _ =
  let gates = process_input "./input.txt" in
  let map = String.Map.of_alist_exn gates in
  let v = Gate.Value (Gate.Register "a") in
  Gate.eval map v |> printf "a: %d\n"