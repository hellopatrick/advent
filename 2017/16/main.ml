open Core

let process_input filename =
  let f channel =
    let parse lexbuf = Parser.moves Lexer.read lexbuf in
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let dance n moves =
  let rec aux step init seen =
    if step = 1_000_000_000 then (String.of_char_list @@ Array.to_list init)
    else
      let prev = (String.of_char_list @@ Array.to_list init) in
      match List.findi seen ~f:(fun _ s -> String.equal s prev) with
      | None ->
        let seen = List.append seen [prev] in
        aux (step+1) (List.fold moves ~init ~f:Dance.perform) seen
      | Some (i, _) ->
        let k = n % step in
        List.nth_exn seen k
  in aux 0 (String.to_array "abcdefghijklmnop") []

let _ =
  let moves = process_input "./input.txt" in
  dance 1_000_000_000 moves
  |> printf "%s\n"