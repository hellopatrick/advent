open Core

type t = { w:int; l:int; h:int; }

let of_string str =
  match String.split str ~on:'x' with
  | [w; l; h;] ->
    let w = Int.of_string w
    and l = Int.of_string l
    and h = Int.of_string h
    in { w; l; h; }
  | _ -> failwith "error parsing."

let surface_area t = (2 * t.w * t.l) + (2 * t.w * t.h) + (2 * t.l * t.h)

let smallest_side_area t =
  match List.sort [t.w; t.l; t.h] ~cmp:Int.compare with 
  | [a; b; c] -> a * b
  | _ -> failwith "something went horribly wrong"

let required_wrapping_paper t =
  let to_cover = surface_area t 
  and slack = smallest_side_area t
  in to_cover + slack

let smallest_perimeter t =
  match List.sort [t.w; t.l; t.h] ~cmp:Int.compare with
  | [a; b; c] -> a + a + b + b
  | _ -> failwith "something went horribly wrong"

let required_ribbon t =
  let bow = t.w * t.l * t.h
  and wrap = smallest_perimeter t
  in bow + wrap