open Core

let read_input filename = 
  filename
  |> In_channel.read_all
  |> String.split_on_chars ~on:[',']
  |> List.map ~f:Int.of_string

type operand =
  | Immediate of int
  | Address of int

type unary_operation = {
  left: operand;
  right: operand;
}

type binary_operation = {
  left: operand;
  right: operand;
  out: int;
}

type instruction =
  | Add of binary_operation
  | Mult of binary_operation
  | Input of int
  | Output of operand
  | Jnz of unary_operation
  | Jez of unary_operation
  | LessThan of binary_operation
  | Equal of binary_operation
  | Halt

let value_of_operand mem op =
  match op with
  | Immediate value -> value
  | Address addr -> Array.get mem addr

let decode_opcode opcode =
  let out_mode = opcode / 10000 in
  let right_mode = (opcode % 10000) / 1000 in
  let left_mode = (opcode % 1000) / 100 in
  let instruction = opcode % 100 in
  (instruction, left_mode, right_mode, out_mode)

exception UnknownOpcode of int
exception UnknownAddressMode of int

let create_operand mode value =
  match mode with
  | 0 -> Address value
  | 1 -> Immediate value
  | x -> raise (UnknownAddressMode x)

let create_binary_op mem index left_mode right_mode =
  {
    left=(create_operand left_mode (Array.get mem (index + 1)));
    right=(create_operand right_mode (Array.get mem (index + 2)));
    out=(Array.get mem (index + 3))
  }

let create_jump_op mem index left_mode right_mode =
 {
   left=(create_operand left_mode (Array.get mem (index + 1)));
   right=(create_operand right_mode (Array.get mem (index + 2)));
 }

let create_instruction mem index =
  let (instruction, left_mode, right_mode, _) = decode_opcode (Array.get mem index) in
  match instruction with
  | 1 -> Add (create_binary_op mem index left_mode right_mode)
  | 2 -> Mult (create_binary_op mem index left_mode right_mode)
  | 3 -> Input (Array.get mem (index + 1))
  | 4 -> Output (create_operand left_mode (Array.get mem (index + 1)))
  | 5 -> Jnz (create_jump_op mem index left_mode right_mode)
  | 6 -> Jez (create_jump_op mem index left_mode right_mode)
  | 7 -> LessThan (create_binary_op mem index left_mode right_mode)
  | 8 -> Equal (create_binary_op mem index left_mode right_mode)
  | 99 -> Halt
  | x -> raise (UnknownOpcode x)

let bool_to_int bool =
  match bool with
  | false -> 0
  |  true -> 1

let execute_one mem (index, input, output) =
  let open Continue_or_stop in
  let instr = create_instruction mem index in
  match instr with
  | Add { left; right; out } -> 
    Array.set mem out ((value_of_operand mem left) + (value_of_operand mem right));
    Continue (index + 4, input, output)
  | Mult { left; right; out } ->
    Array.set mem out ((value_of_operand mem left) * (value_of_operand mem right));
    Continue (index + 4, input, output)
  | Input addr ->
    Array.set mem addr (List.hd_exn input);
    Continue (index + 2, List.tl_exn input, output)
  | Output operand ->
    let value = value_of_operand mem operand in
    Continue (index + 2, input, (value :: output))
  | Jnz { left; right } -> 
    if (value_of_operand mem left) <> 0 then
      Continue (value_of_operand mem right, input, output)
    else
      Continue (index + 3, input, output)
  | Jez { left; right } -> 
    if (value_of_operand mem left) = 0 then
      Continue (value_of_operand mem right, input, output)
    else
      Continue (index + 3, input, output)
  | LessThan { left; right; out } ->
    Array.set mem out (bool_to_int ((value_of_operand mem left) < (value_of_operand mem right)));
    Continue (index + 4, input, output)
  | Equal { left; right; out } ->
    Array.set mem out (bool_to_int ((value_of_operand mem left) = (value_of_operand mem right)));
    Continue (index + 4, input, output)
  | Halt -> Stop (index, input, output)

let rec execute mem state =
  let open Continue_or_stop in
  match execute_one mem state with
  | Continue next_state -> execute mem next_state
  | Stop (_, _, out) -> out

let print_list list =
  let _ = List.map ~f:(fun x -> printf "%d " x) list in
  printf "\n"

let () =
  let mem = read_input "./day5/input.txt" in
  let out = execute (Array.of_list mem) (0, [1], []) in
  printf "Part 1: %d\n" (List.hd_exn out);
  let out = execute (Array.of_list mem) (0, [5], []) in
  printf "Part 2: %d\n" (List.hd_exn out);