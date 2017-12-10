open Core

let parse lexbuf = Parser.instructions Lexer.read lexbuf

let process_input filename =
  let f channel =
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let _ =
  let instructions = process_input "./instructions.txt" in
  let init = State.{mem = String.Map.empty; max = 0} in
  let final = List.fold instructions ~init ~f:State.run in
  match Map.data final.mem |> List.max_elt ~cmp:Int.compare with
  | Some v -> printf "current_max = %d; all_time_max = %d" v final.max
  | None -> printf "something went wrong."