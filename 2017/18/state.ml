open Core

type s = Running | Waiting | Terminated

type t = {
  name: int;
  registers: int Char.Map.t;
  send_queue: int Queue.t;
  recv_queue: int Queue.t;
  line: int;
  sent: int;
  state: s;
}

let create p send_queue recv_queue =
  let registers = Char.Map.add (Char.Map.empty) ~key:'p' ~data:p in
  {name=p; registers; send_queue; recv_queue; line=0; sent=0; state=Running}

let to_string t =
  sprintf "%d: %d - %d" t.name t.line t.sent

let value_in_register t c =
  Char.Map.find t.registers c
  |> Option.value ~default:0

let value t v =
  let open Instruction in
  match v with
  | Value i -> i
  | Register c -> value_in_register t c

let do_set t c data =
  {t with registers=(Char.Map.add t.registers ~key:c ~data); line=(t.line + 1)}

let set t c v =
  do_set t c (value t v)

let multiply t c v =
  let init = (value_in_register t c) in
  let v = (value t v) in
  do_set t c (init * v)

let add t c v =
  let init = (value_in_register t c) in
  let v = (value t v) in
  do_set t c (init + v)

let modulus t c v =
  let init = (value_in_register t c) in
  let v = (value t v) in
  do_set t c (init mod v)

let send t c =
  let data = value_in_register t c in
  Queue.enqueue t.send_queue data;
  {t with sent=(t.sent + 1); line=(t.line + 1)}

let receive t c =
  let state_if_nothing = function
    | Waiting | Terminated -> Terminated
    | Running -> Waiting in
  match Queue.dequeue t.recv_queue with
  | None -> {t with state=state_if_nothing t.state}
  | Some n -> {(do_set t c n) with state=Running}

let jump t c v =
  let init = (value t c) in
  let v = (value t v) in
  let jump = if init > 0 then v else 1 in
  {t with line = (t.line + jump)}

let exec t instruction =
  match instruction with
  | Instruction.Send c -> send t c
  | Instruction.Set (c,v) -> set t c v
  | Instruction.Multiply (c,v) -> multiply t c v
  | Instruction.Add (c,v) -> add t c v
  | Instruction.Modulus (c,v) -> modulus t c v
  | Instruction.Receive (c) -> receive t c
  | Instruction.Jump (c, v) -> jump t c v

let execute t instructions =
  if t.line < 0 || t.line >= Array.length instructions then {t with state=Terminated}
  else exec t instructions.(t.line)