open Core
open Pipe

let rec travel map visited n =
  if Int.Set.mem visited n then visited
  else
    let visited = Int.Set.add visited n in
    let travel' = travel map in
    let children = Int.Map.find_exn map n in
    let f visited child = Int.Set.union (travel' visited child) visited in
    List.fold children ~init:visited ~f

let groups set map =
  let rec aux unvisited map groups =
    match Int.Set.choose unvisited with
    | None -> groups
    | Some r ->
      let new_group = travel map (Int.Set.empty) r in
      aux (Set.diff unvisited new_group) map (new_group::groups)
  in aux set map []

let process_input filename =
  let f channel =
    let parse lexbuf = Parser.pipes Lexer.read lexbuf in
    let lexer_buffer = Lexing.from_channel channel in
    lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename};
    parse lexer_buffer
  in In_channel.with_file filename ~f

let _ =
  let pipes = process_input "./pipes.txt" in
  let create_map acc pipe = Int.Map.add acc ~key:pipe.n ~data:pipe.links in
  let pipe_map = List.fold pipes ~init:Int.Map.empty ~f:create_map in

  let zero_group = travel pipe_map (Int.Set.empty) 0 in
  printf "zeroth group: %d\n" (Set.length zero_group);

  let create_set acc pipe = Int.Set.add acc pipe.n in
  let unvisited = List.fold pipes ~init:Int.Set.empty ~f:create_set in
  let groups = groups unvisited pipe_map in
  printf "groups: %d\n" (List.length groups)