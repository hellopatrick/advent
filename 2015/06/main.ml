open Core

let determine_brightness lights =
  let count acc row = acc + Array.fold row ~init:0 ~f:Int.(+) in
  Array.fold lights ~init:0 ~f:count

let process_input filename =
  let f channel =
    let parse lexbuf = Parser.instructions Lexer.read lexbuf in
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let _ =
  let instructions = process_input "./input.txt" in
  let lights = Array.make_matrix ~dimx:1000 ~dimy:1000 0 in
  Instruction.follow instructions lights;
  let on_lights = determine_brightness lights in
  printf "brightness: %d\n" on_lights