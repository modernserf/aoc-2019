open Core

let read_input filename = 
  filename
  |> In_channel.read_all
  |> String.split_on_chars ~on:[',']
  |> List.map ~f:Int.of_string

type node_state = {
  mem: int array; 
  index: int;
  input: int list;
  output: int list;
  relative_base: int;
}

let create_state list input = 
  let z = Array.create ~len:1000 0 in
  let mem = Array.of_list list in
  {
    mem = Array.append mem z;
    index = 0;
    input = input;
    output = [];
    relative_base = 0
  }


(* operands *)
type operand =
  | Immediate of int
  | Address of int
  | Relative of int

exception UnknownAddressMode of int

let create_operand mode value =
  match mode with
  | 0 -> Address value
  | 1 -> Immediate value
  | 2 -> Relative value
  | x -> raise (UnknownAddressMode x)

let value_of_operand state op =
  match op with
  | Address addr -> Array.get state.mem addr
  | Immediate value -> value
  | Relative addr -> 
    Array.get state.mem (addr + state.relative_base)

let value_of_write_operand state op = 
  match op with
  | Address addr -> addr
  | Immediate value -> value
  | Relative value -> value + state.relative_base

(* instructions *)
type unary_operation = {
  left: operand;
  right: operand;
}

type binary_operation = {
  left: operand;
  right: operand;
  out: operand;
}

type instruction =
  | Add of binary_operation
  | Mult of binary_operation
  | Input of operand
  | Output of operand
  | Jnz of unary_operation
  | Jez of unary_operation
  | LessThan of binary_operation
  | Equal of binary_operation
  | SetRelativeBase of operand
  | Halt

let create_op state mode offset =
  create_operand mode (Array.get state.mem (state.index + offset))

let create_binary_op state left_mode right_mode out_mode =
  {
    left=create_op state left_mode 1;
    right=create_op state right_mode 2;
    out=create_op state out_mode 3;
  }

let create_jump_op state left_mode right_mode =
 {
    left=create_op state left_mode 1;
    right=create_op state right_mode 2;
 }

let decode_opcode opcode =
  let out_mode = opcode / 10000 in
  let right_mode = (opcode % 10000) / 1000 in
  let left_mode = (opcode % 1000) / 100 in
  let instruction = opcode % 100 in
  (instruction, left_mode, right_mode, out_mode)

exception UnknownOpcode of int

let create_instruction state =
  let { mem; index; _ } = state in
  let (instruction, left_mode, right_mode, out_mode) = decode_opcode (Array.get mem index) in
  match instruction with
  | 1 -> Add (create_binary_op state left_mode right_mode out_mode)
  | 2 -> Mult (create_binary_op state left_mode right_mode out_mode)
  | 3 -> Input (create_op state left_mode 1)
  | 4 -> Output (create_op state left_mode 1)
  | 5 -> Jnz (create_jump_op state left_mode right_mode)
  | 6 -> Jez (create_jump_op state left_mode right_mode)
  | 7 -> LessThan (create_binary_op state left_mode right_mode out_mode)
  | 8 -> Equal (create_binary_op state left_mode right_mode out_mode)
  | 9 -> SetRelativeBase (create_op state left_mode 1)
  | 99 -> Halt
  | x -> raise (UnknownOpcode x)

let bool_to_int bool =
  match bool with
  | false -> 0
  |  true -> 1

type 't coroutine = 
  | Continue of 't
  | Stop of 't
  | Yield of 't

let step state steps = 
  { state with index = (state.index + steps) }

let set_binary state { left; right; out } fn = 
  Array.set state.mem (value_of_write_operand state out) (fn (value_of_operand state left) (value_of_operand state right))

let execute_one state =
  let { mem; index; input; output; _ } = state in
  let instr = create_instruction state in
  match instr with
  | Add op -> 
    set_binary state op (+);
    Continue (step state 4)
  | Mult op ->
    set_binary state op ( * );
    Continue (step state 4)
  | Input operand -> (
    match input with
    | [] -> Yield state
    | h :: t -> 
      let value = value_of_write_operand state operand in
      Array.set mem value h;
      Continue { state with index = index + 2; input = t }
    )
  | Output operand ->
    let value = value_of_operand state operand in
    Continue  { state with index = index + 2; output = (value :: output) }
  | Jnz { left; right } -> 
    if (value_of_operand state left) <> 0 then
      Continue { state with index = value_of_operand state right }
    else
      Continue (step state 3)
  | Jez { left; right } -> 
    if (value_of_operand state left) = 0 then
      Continue { state with index = value_of_operand state right }
    else
      Continue (step state 3)
  | LessThan op ->
    set_binary state op (fun l r -> bool_to_int (l < r));
    Continue (step state 4)
  | Equal op ->
    set_binary state op (fun l r -> bool_to_int (l = r));
    Continue (step state 4)
  | SetRelativeBase operand ->
    let value = value_of_operand state operand in
    Continue { state with relative_base = state.relative_base + value; index = index + 2 }
  | Halt -> 
    Stop state

let rec execute state =
  match execute_one state with
  | Continue next_state -> execute next_state
  | Stop next_state -> next_state.output
  | Yield next_state -> next_state.output

let print_list list =
  let _ = List.map ~f:(fun digit -> printf "%d " digit) list in
  ()

let () =
  let program = read_input "./day9/input.txt" in
  let state = create_state program [1] in
  let output = execute state in

  printf "Part 1:";
  print_list (List.rev output);
  printf "\n";

  let state = create_state program [2] in
  let output = execute state in
  printf "Part 2:";
  print_list (List.rev output);
  printf "\n";
