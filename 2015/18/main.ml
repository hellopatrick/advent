open Core

module State = struct
  type t = On | Off

  let of_char = function
    | '#' -> On
    | _ -> Off

  let transition s n =
    match s with
    | On -> if n = 2 || n = 3 then On else Off
    | Off -> if n = 3 then On else Off
end

module Grid = struct
  type t = State.t array array

  let neighbors t x y =
    let get t x y =
      try t.(y).(x)
      with _ -> State.Off
    in
    [ get t (x-1) (y-1); get t (x) (y-1); get t (x+1) (y-1);
      get t (x-1) (y);                    get t (x+1) (y);
      get t (x-1) (y+1); get t (x) (y+1); get t (x+1) (y+1); ]

  let force_corners_on t =
    let len = Array.length t in
    t.(0).(0) <- State.On;
    t.(0).(len-1) <- State.On;
    t.(len-1).(0) <- State.On;
    t.(len-1).(len-1) <- State.On;
    t

  let update t =
    let neighbors = neighbors t in
    let on_neighbors x y =
      List.count (neighbors x y) ~f:(fun s -> s = State.On)
    in
    let update_rows y row =
      let update_cols x s =
        let n = on_neighbors x y in
        State.transition s n
      in Array.mapi row ~f:update_cols
    in Array.mapi t ~f:update_rows

  let ons t =
    Array.map t ~f:(fun row -> Array.count row ~f:(fun s -> s = State.On))
    |> Array.fold ~init:0 ~f:Int.(+)
end

let parse_input () =
  let parse_lines line =
    String.to_array line |> Array.map ~f:State.of_char
  in
  In_channel.read_lines "./input.txt"
  |> List.map ~f:parse_lines
  |> List.to_array

let _ =
  let init = parse_input () in
  let result = Fn.apply_n_times ~n:100 Grid.update init in
  Grid.ons result
  |> printf "a: %d\n";

  let init = parse_input () |> Grid.force_corners_on in
  let update = Fn.compose Grid.force_corners_on Grid.update in
  let result = Fn.apply_n_times ~n:100 update init in
  Grid.ons result
  |> printf "b: %d\n"