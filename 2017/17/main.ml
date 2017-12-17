open Core

module CB = Circularbuffer

let steps = 349

let part_a buffer rounds =
  let rec aux buffer n =
    if n = 0 then buffer
    else aux (CB.step buffer steps) (n-1)
  in
  let result = aux buffer rounds in
  let n = CB.find_after result rounds in
  printf "a: %d\n" n

let part_b after_zero stop =
  let rec aux loc len =
    if len < stop then
      let curr = (loc + steps) % len in
      if curr = 0 then after_zero := len;
      aux (curr + 1) (len + 1)
  in aux 0 1

let _ =
  let buffer = CB.create 0 in
  part_a buffer 2017;

  let value = ref 0 in
  part_b value 50_000_000;
  printf "b: %d\n" (!value);
