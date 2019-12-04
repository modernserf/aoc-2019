open Core

let input = (153517, 630395)

let rec int_to_digits ~state:[] num =
  if num = 0 then state else
  let next = num / 10 in
  let digit = num - (next * 10) in
  int_to_digits ~state:(digit :: list) next

let rec is_sorted ~state:0 digits =
  match digits with
  | [] -> true
  | (hd :: tl) -> (state <= hd) && (is_sorted ~state:hd tl)

let rec has_adjacency ~state:0 digits =
  match digits with
  | [] -> false
  | (hd :: tl) -> (state = hd) || (has_adjacency ~state:hd tl)

let rec range ~state:[] min max =
  if min = max then state else
  range ~state:(max :: state) min (max - 1)

let possible_passwords min max =
  range min max
  |> List.map ~f:int_to_digits
  |> List.filter ~f:(fun digits -> (is_sorted digits) && (has_adjacency digits))

let () =
  let (min, max) = input in
  let candidates = possible_passwords min max in
  printf "Part 1: %d\n" (List.length candidates);