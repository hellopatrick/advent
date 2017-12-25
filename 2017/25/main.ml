open Core

let rec loop state n =
  if n = 0 then state
  else loop (Machine.transition state) (n-1)

let _ =
  let state = Machine.{tape=Int.Map.empty; current_state=A; cursor=0} in
  let final = loop state 12_667_664 in
  Int.Map.data final.tape
  |> List.count ~f:(fun x -> x = 1)
  |> printf "%d\n"
