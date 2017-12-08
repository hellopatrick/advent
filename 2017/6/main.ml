open Core

let max ary =
    let f i (k, m) n = if n > m then (i, n) else (k, m)
    and init = (0, 0) in
    Array.foldi ary ~init ~f

let rec redistribute ary n i =
    if n = 0 then ()
    else
        let i = i mod Array.length ary in
        ary.(i) <- (ary.(i) + 1);
        redistribute ary (n - 1) (i + 1)

let compute ary =
    let seen = Sexp.Map.empty in
    let rec aux ary seen k =
        let i, max = max ary in
        ary.(i) <- 0;
        redistribute ary max (i + 1);
        let sexp = Array.sexp_of_t Int.sexp_of_t ary in
        match Sexp.Map.find seen sexp with
        | Some n -> (k, k - n)
        | None -> aux ary (Sexp.Map.add seen ~key:sexp ~data:k) (k + 1)
    in aux ary seen 1

let parse_input () =
    In_channel.read_all "./registers.txt"
    |> String.split ~on:'\t'
    |> List.map ~f:Int.of_string
    |> Array.of_list

let _ =
    let input = parse_input () in
    let a, b = compute input in
    printf "a: %d\n" a;
    printf "b: %d\n" b;
