open Core

    (*

    this is the assembly translated to C-like.
    it is looking for non-primes in { k | i >= 0 AND b + 17 * i < c }

    b = 65*100 + 100_000
    c = b + 17_000

    for b=65; b < c; b += 17 {
        f = 1
        d = 2
        e = 2

        for d=2; d < b; d++ {
            for e=2; e < b; e++ {
                if b = d * e {
                    f = 0
                }
            }
        }

        if(f==0) {
            h+=1
        }
    }

    optimizations:
    - break loop early.
    - only really need to check b % d = 0
    - only check d from 2 to sqrt(b)

    *)

let b = 65 * 100 + 100_000
let c = b + 17_000

let prime k =
  let rec loop b k =
    if b * b > k then true
    else if k % b = 0 then false
    else loop (b+1) k
  in loop 2 k

let rec loop k c h =
  if k > c then h
  else
  if prime k then loop (k+17) c h
  else loop (k+17) c (h+1)

let _ =
  let values = loop b c 0 in
  printf "part b: %d\n" values