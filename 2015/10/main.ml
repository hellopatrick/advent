open Core

let look_and_say chars =
  let rec aux chars l (last, n) =
    match chars with
    | [] -> List.rev (last::n::l)
    | hd::tl ->
      if hd = last then aux tl l (last, n+1)
      else aux tl (last::n::l) (hd, 1)
  in
  let next = aux chars [] (0, 0) in
  List.drop next 2

let _ =
  let input = String.to_list "1113222113" |> List.map ~f:Char.get_digit_exn in
  let rec loop input n =
    if n = 0 then input
    else loop (look_and_say input) (n-1)
  in loop input 50
     |> List.length
     |> printf "%d\n"