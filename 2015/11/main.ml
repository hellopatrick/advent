open Core

module Password = struct
  type t = string

  let no_confusing_letters t =
    not ((String.contains t 'l') || (String.contains t 'i') || (String.contains t 'o'))

  let contains_increasing t =
    let rec aux = function
      | a::b::c::tl ->
        if (b = a+1 && c = b+1) then true else aux (b::c::tl)
      | _ -> false
    in aux (String.to_list t |> List.map ~f:Char.to_int)

  let contains_doubles t = 
    let rec aux chars last set = 
      match chars with
      | [] -> set
      | c::tl ->
        let set = if c = last then (Char.Set.add set c) else set in
        aux tl c set
    in
    let chars = String.to_list t in
    let hd = List.hd_exn chars in
    let tl = List.tl_exn chars in
    let double_chars = aux tl hd Char.Set.empty in
    (Set.length double_chars) > 1

  let is_valid t =
    (no_confusing_letters t) && (contains_increasing t) && (contains_doubles t)

  let increment t =
    let next_letter = function
      | 'z' -> 'a'
      | 'a'..'y' as c -> Char.of_int_exn ((Char.to_int c) + 1)
      | _ -> failwith "unsupported char."
    in
    let rec aux = function
      | [] -> []
      | 'z'::tl -> 'a' :: aux tl
      | c::tl -> (next_letter c) :: tl
    in let inc = aux (String.to_list_rev t) in
    List.rev inc |> String.of_char_list
end

let rec find_next_password password =
  let next = (Password.increment password) in
  if Password.is_valid next then next
  else find_next_password next

let _ =
  find_next_password "cqjxjnds"
  |> printf "%s\n"
