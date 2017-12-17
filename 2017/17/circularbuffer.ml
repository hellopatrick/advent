open Core

type 'a t = { buffer:'a Doubly_linked.t; location:'a Doubly_linked.Elt.t; current:'a}

let value_exn = function
  | Some n -> n
  | None -> failwith "no element."

let create init =
  let buffer = Doubly_linked.create () in
  let location = Doubly_linked.insert_first buffer init in
  {buffer; location; current=(init+1)}

let go t n =
  let rec aux t n =
    if n = 0 then t
    else
      let next = match Doubly_linked.next t.buffer t.location with
        | None -> (Doubly_linked.first_elt t.buffer) |> value_exn
        | Some elt -> elt
      in aux {t with location=next;} (n-1)
  in aux t n

let insert t =
  let new_location = Doubly_linked.insert_after t.buffer t.location t.current in
  {t with location=new_location;
          current=(t.current + 1)}

let step t n =
  go t n |> insert

let find t n =
  Doubly_linked.find_elt t.buffer ~f:(Int.equal n)

let find_after b n =
  match Option.bind (find b n) ~f:(Doubly_linked.next b.buffer) with
  | None -> Doubly_linked.first b.buffer |> (Option.value ~default:0)
  | Some elt -> Doubly_linked.Elt.value elt

let to_string t =
  Doubly_linked.sexp_of_t Int.sexp_of_t t.buffer
  |> Sexp.to_string