open Core

let _ =
  let state = Machine.{tape=Int.Map.empty; current_state=A; cursor=0} in
  let final = Fn.apply_n_times ~n:12_667_664 Machine.transition state in
  Int.Map.data final.tape
  |> List.count ~f:(fun x -> x = 1)
  |> printf "%d\n"
