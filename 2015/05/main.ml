open Core

let has_forbidden str =
  let is_substring = String.is_substring str in
  (is_substring "ab") || (is_substring "cd") || (is_substring "pq") || (is_substring "xy")

let count_vowels str =
  let is_vowel = function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false
  in String.count str ~f:is_vowel

let has_repeat chars =
  match List.find_consecutive_duplicate chars ~equal:Char.equal with
  | Some _ -> true
  | None -> false

let is_nice_a str =
  if has_forbidden str then false
  else
    (count_vowels str >= 3) && (has_repeat (String.to_list str))

let has_repeat_pair chars =
  let rec aux = function
    | [] | [_] | [_; _] | [_; _; _;]-> false
    | a::b::tl ->
      let tail = String.of_char_list tl in
      let substring = String.of_char_list [a; b] in
      if String.is_substring tail ~substring then true
      else aux (b::tl)
  in aux chars

let has_repeat_spaced_char chars =
  let rec aux = function
    | [] | [_] | [_; _] -> false
    | a::b::c::tl ->
      if a = c then true
      else aux (b::c::tl)
  in aux chars

let is_nice_b str =
  let str = String.to_list str in
  (has_repeat_pair str) && (has_repeat_spaced_char str)

let parse_input () =
  In_channel.read_lines "./input.txt"

let _ =
  let strings = parse_input () in
  printf "nice for a: %d\n" (List.count strings ~f:is_nice_a);
  printf "nice for b: %d" (List.count strings ~f:is_nice_b)