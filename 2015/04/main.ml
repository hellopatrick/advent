open Core

let part_a key =
  let rec aux n =
    let md5 = Digest.string (sprintf "%s%d" key n) |> Digest.to_hex in
    if String.is_prefix md5 ~prefix:"00000" then n
    else aux (n + 1)
  in aux 1

let part_b key =
  let rec aux n =
    let md5 = Digest.string (sprintf "%s%d" key n) |> Digest.to_hex in
    if String.is_prefix md5 ~prefix:"000000" then n
    else aux (n + 1)
  in aux 1

let _ =
  let key = "yzbqklnj" in

  let a = part_a key in
  printf "a: %d\n" a;

  let b = part_b key in
  printf "b: %d\n" b;