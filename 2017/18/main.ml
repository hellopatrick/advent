open Core

let run instructions active waiting =
  let open State in
  let rec aux active other =
    match active.state, other.state with
    | Terminated, Terminated -> active, other
    | Terminated, _ -> aux (State.execute other instructions) active
    | Waiting, _ -> aux (State.execute other instructions) active
    | Running, _ -> aux (State.execute active instructions) other
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

  let queue_one = Queue.create () in
  let queue_two = Queue.create () in

  let zero = State.create 0 queue_one queue_two in
  let one = State.create 1 queue_two queue_one in

  let a, b = run instructions zero one in
  State.(
    printf "prog: %d, sent: %d\n" a.name a.sent;
    printf "prog: %d, sent: %d\n" b.name b.sent;
  )