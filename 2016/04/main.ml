open Core

type room = { 
  id:string list; 
  sector: int; 
  checksum:char list
}

let convert _ = {
  id = ["s"]; 
  sector = 0; 
  checksum = ['a']
}

let valid _ = false

let _ = 
  In_channel.read_lines "./input.txt"
  |> List.map ~f:convert
  |> List.filter ~f:valid 
  |> List.fold ~init:0 ~f:(fun acc room -> acc + room.sector)
  |> printf "%d"