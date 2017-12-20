open Core

let table = Vector.Table.create () ~size:1000

let loop particles =
  let step = Array.iter ~f:Particle.step in
  let remove_collisions particles =
    Vector.Table.clear table;
    let f p =
      Vector.Table.add_multi table ~key:Particle.(p.p) ~data:1
    in Array.iter particles ~f;

    let only_lonely_particles p =
      match Vector.Table.find table Particle.(p.p) with
      | Some [l] -> true
      | _ -> false
    in Array.filter particles ~f:only_lonely_particles in
  let rec aux particles i j last_change =
    if last_change > 10000 then exit 0;
    step particles;
    let new_particles = remove_collisions particles in
    let count = (Array.length new_particles) in
    match count <> i with
    | true ->
      printf "t=%02d; collisions=%02d; now=%03d\n" j (i-count) count; Out_channel.flush stdout;
      aux new_particles count (j+1) 0
    | false -> aux new_particles i (j+1) (last_change+1)
  in aux particles (Array.length particles) 0 0

let process_input filename =
  let f channel =
    let parse lexbuf = Parser.particles Lexer.read lexbuf in
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let _ =
  let particles = process_input "./input.txt" |> List.to_array in
  loop particles