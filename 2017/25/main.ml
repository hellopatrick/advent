open Core

let _ =
  let state = Machine.{tape=Int.Set.empty; current_state=A; cursor=0} in
  let final = Fn.apply_n_times ~n:12_667_664 Machine.transition state in
  Int.Set.length final.tape
  |> printf "%d\n";
