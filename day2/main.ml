open Core

let data = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0"
let target = 19690720

(* used for debugging *)
let print_arr arr =
  let _ = Array.map ~f:(fun x -> printf "%d " x) arr in
  printf "\n"

let input_to_array str = 
  str 
  |> String.split_on_chars ~on:[',']
  |> List.map ~f:Int.of_string
  |> List.to_array

exception UnknownOpcode of int

let do_op opcode lhs rhs =
  match opcode with
  | 1 -> lhs + rhs
  | 2 -> lhs * rhs
  | _ -> raise (UnknownOpcode opcode)

let get_indirect arr idx =
  Array.get arr (Array.get arr idx)

let set_indirect arr idx value =
  Array.set arr (Array.get arr idx) value

let interpret_at arr pos =
  let opcode = Array.get arr pos in
  if opcode = 99 then None else
  let lhs = get_indirect arr (pos + 1) in
  let rhs = get_indirect arr (pos + 2) in
  set_indirect arr (pos + 3)  (do_op opcode lhs rhs);
  Some (pos + 4)

let rec interpret arr pos =
  match interpret_at arr pos with
  |  None -> Array.get arr 0
  |  Some next_pos -> interpret arr next_pos

let run_program data noun verb =
  let arr = input_to_array data in
  Array.set arr 1 noun;
  Array.set arr 2 verb;
  interpret arr 0

let rec run_program_until data target noun verb =
  let result = run_program data noun verb in
  if target = result then (noun, verb) else
  if noun = verb then run_program_until data target (noun + 1) 0 else
  run_program_until data target noun (verb + 1)

let () =
  let part1 = run_program data 12 2 in
  printf "Part 1: %d\n" part1;
  let (noun, verb) = run_program_until data target 0 0 in
  printf "Part 2: %d\n" (100 * noun + verb);