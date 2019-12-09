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

type 't coroutine = 
  | Continue of 't
  | Stop of 't
  | Yield of 't

type node_state = {
  mem: int array; 
  index: int;
  input: int list;
  output: int list;
  initial_phase: int;
}

let init_node_state program phase =
  { 
    mem = Array.of_list program;
    index = 0;
    input = [phase];
    output = [];
    initial_phase = phase;
  }

let step state steps = 
  { state with index = (state.index + steps) }

let execute_one state =
  let { mem; index; input; output; _ } = state in
  let instr = create_instruction mem index in
  match instr with
  | Add { left; right; out } -> 
    Array.set mem out ((value_of_operand mem left) + (value_of_operand mem right));
    Continue (step state 4)
  | Mult { left; right; out } ->
    Array.set mem out ((value_of_operand mem left) * (value_of_operand mem right));
    Continue (step state 4)
  | Input addr -> (
    match input with
    | [] -> Yield state
    | h :: t -> 
      Array.set mem addr h;
      Continue { state with index = index + 2; input = t }
    )
  | Output operand ->
    let value = value_of_operand mem operand in
    Continue  { state with index = index + 2; output = (value :: output) }
  | Jnz { left; right } -> 
    if (value_of_operand mem left) <> 0 then
      Continue { state with index = value_of_operand mem right }
    else
      Continue (step state 3)
  | Jez { left; right } -> 
    if (value_of_operand mem left) = 0 then
      Continue { state with index = value_of_operand mem right }
    else
      Continue (step state 3)
  | LessThan { left; right; out } ->
    Array.set mem out (bool_to_int ((value_of_operand mem left) < (value_of_operand mem right)));
    Continue (step state 4)
  | Equal { left; right; out } ->
    Array.set mem out (bool_to_int ((value_of_operand mem left) = (value_of_operand mem right)));
    Continue (step state 4)
  | Halt -> 
    Stop state

let rec execute state =
  match execute_one state with
  | Continue next_state -> execute next_state
  | Stop next_state -> next_state.output
  | Yield next_state -> next_state.output

let execute_cluster program input =
  let mem = List.to_array program in
  execute { mem; index = 0; input; output = []; initial_phase = 0 }

let append_input next_state input = {
  next_state with 
  input = (List.append next_state.input input) 
}

let pair_exn list = 
  (List.hd_exn list, List.tl_exn list)

let rec execute_cluster_feedback_loop (states : node_state list) : int list =
  let (state, rest_states) = pair_exn states in
  match execute_one state with
  | Stop end_state -> (
    match rest_states with
    | [] -> end_state.output
    | next_state :: tail ->
      let next_state_with_prev_output = append_input next_state end_state.output in
      execute_cluster_feedback_loop (next_state_with_prev_output :: tail)
  )
  | Continue next_state -> 
    execute_cluster_feedback_loop (next_state :: rest_states)
  | Yield yield_state ->
    let (next_state, tail) = pair_exn rest_states in
    let next_state_with_prev_output = append_input next_state yield_state.output in
    let next_tail = List.append tail [{ yield_state with output = [] }] in
    execute_cluster_feedback_loop (next_state_with_prev_output :: next_tail)

let execute_cluster_feedback (program : int list) (input : int list) : int =
  let (first, rest) = pair_exn (List.map ~f:(init_node_state program) input) in 
  let states = (append_input first [0]) :: rest in
  List.hd_exn (execute_cluster_feedback_loop states)

let signal_level program value =
  value
  |> List.fold 
    ~init:[0]
    ~f:(fun input digit -> execute_cluster program (digit :: input))
  |> List.hd_exn

(* interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ] *)
let rec interleave x lst = 
  match lst with
  | [] -> [[x]]
  | hd::tl -> (x::lst) :: (List.map ~f:(fun y -> hd::y) (interleave x tl))

(*permutations [1; 2; 3] = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] *)
let rec permutations lst = 
  match lst with
  | hd::tl -> List.concat (List.map ~f:(interleave hd) (permutations tl))
  | _ -> [lst]

let rec max_list ?(state=0) list =
  match list with
  | [] -> state
  | hd :: tl -> 
    max_list
      ~state:(if hd > state then hd else state)
      tl

let print_list list =
  let _ = List.map ~f:(fun digit -> printf "%d" digit) list in
  ()

let () =
  let program = read_input "./day7/input.txt" in
  let inputs = permutations [0;1;2;3;4] in
  let results = List.map ~f:(signal_level program) inputs in
  printf "Part 1: %d\n" (max_list results);
  let inputs = permutations [5;6;7;8;9] in
  let results = List.map ~f:(execute_cluster_feedback program) inputs in
  printf "Part 2: %d\n" (max_list results);
