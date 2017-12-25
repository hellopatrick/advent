open Core

module Position = struct
  type state = Flying | Resting

  type t = { reindeer:Reindeer.t; state:state; time_in_state:int; flight_distance:int; mutable points:int; }

  let compare a b =
    Int.compare a.flight_distance b.flight_distance

  let compare_points a b =
    Int.compare a.points b.points

  let create reindeer =
    { reindeer; state=Flying; time_in_state=0; flight_distance=0; points=0; }

  let update position =
    let reindeer = position.reindeer in
    let time_in_state = position.time_in_state + 1 in
    match position.state with
    | Flying ->
      let flight_distance = position.flight_distance + reindeer.speed in
      if time_in_state = reindeer.time_to_fly then { position
                                                     with state=Resting;
                                                          time_in_state=0;
                                                          flight_distance; }
      else { position
             with time_in_state;
                  flight_distance; }
    | Resting ->
      if time_in_state = reindeer.time_to_rest then { position
                                                      with state=Flying;
                                                           time_in_state=0; }
      else { position
             with time_in_state; }
end

let race reindeers n =
  let rec aux positions n =
    if n = 0 then positions
    else begin
      let positions = List.map positions ~f:Position.update in
      let _ = match List.max_elt positions ~cmp:Position.compare with
        | Some winner -> winner.points <- (winner.points + 1)
        | None -> ()
      in aux positions (n-1)
    end
  in
  let positions = List.map reindeers ~f:Position.create in
  aux positions n


let parse_input () =
  In_channel.read_lines "./input.txt"
  |> List.filter_map ~f:Reindeer.of_string

let _ =
  let reindeers = parse_input () in
  let results = race reindeers 2503 in
  let points_winner = List.max_elt results ~cmp:Position.compare_points in
  let distance_winner = List.max_elt results ~cmp:Position.compare in

  match points_winner, distance_winner with
  | Some pw, Some dw ->
    printf "%s won with %d points!\n" pw.reindeer.name pw.points;
    printf "%s won with %d km!" dw.reindeer.name dw.flight_distance;
  | _, _ -> printf "something went wrong!";
