open Core

let run instructions active waiting =
  let open State in
  let rec aux active other =
    match active.state, other.state with
    | Terminated, Terminated -> active, other
    | Terminated, _ -> aux (State.execute other instructions) active
    | Running, _ -> aux (State.execute active instructions) other
    | Waiting, _ -> aux (State.execute other instructions) active
  in aux active waiting

let process_input filename =
  let f channel =
    let parse lexbuf = Parser.instructions Lexer.read lexbuf in
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let _ =
  let instructions = process_input "./input.txt" |> List.to_array in
  let zero_in = Queue.create () in
  let zero_out = Queue.create () in

  let zero = State.create 0 zero_out zero_in in
  let one = State.create 1 zero_in zero_out in
  let open State in
  let a, b = run instructions zero one in
  printf "%d -> %d\n" a.name a.sent;
  printf "%d -> %d\n" b.name b.sent;