open Core

let inspect l = 
  let _ = List.iter ~f:print_endline in 
  l

let valid triangle = 
  match List.sort triangle ~compare:compare with 
  | [a;b;c] -> a + b > c 
  | _ -> false

let convert line = 
  String.strip line
  |> String.split ~on:' '
  |> inspect
  |> List.filter ~f:(fun x -> x <> "")
  |> List.map ~f:Int.of_string

let count list = 
  List.chunks_of list ~length:3
  |> List.count ~f:valid

let _ =
  let transposed_input = In_channel.read_lines "./input.txt"
                         |> List.map ~f:convert
                         |> List.transpose in 
  let out = match transposed_input with 
    | Some m -> List.map ~f:count m |> List.reduce ~f:(+)
    | None -> None in 
  match out with 
  | Some count -> printf "%d" count 
  | None -> ()