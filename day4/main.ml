open Core

let input = (153517, 630395)

let rec int_to_digits ?(state=[]) num =
  if num = 0 then state else
  let next = num / 10 in
  let digit = num - (next * 10) in
  int_to_digits ~state:(digit :: state) next

let rec is_sorted ?(state=0) digits =
  match digits with
  | [] -> true
  | (hd :: tl) -> (state <= hd) && (is_sorted ~state:hd tl)

let rec has_digit_pair ?(state=0) digits =
  match digits with
  | [] -> false
  | (hd :: tl) -> (state = hd) || (has_digit_pair ~state:hd tl)

type digitPairState =
  | Init
  | OneDigit of int
  | TwoDigit of int
  | MoreDigit of int

let next_digit state digit =
  let open Continue_or_stop in
  match state with
  | Init -> Continue (OneDigit digit)
  | OneDigit prev -> if digit = prev then Continue (TwoDigit digit) else Continue (OneDigit digit)
  | TwoDigit prev -> if digit = prev then Continue (MoreDigit digit) else Stop (TwoDigit digit)
  | MoreDigit prev -> if digit = prev then Continue (MoreDigit digit) else Continue (OneDigit digit)

let has_only_digit_pair digits =
  match List.fold_until ~f:next_digit ~init:Init ~finish:ident digits with
  | TwoDigit _ -> true
  | _ -> false

let rec range ?(state=[]) min max =
  if min = max then state else
  range ~state:(max :: state) min (max - 1)

let possible_passwords min max =
  range min max
  |> List.map ~f:int_to_digits
  |> List.filter ~f:(fun digits -> (is_sorted digits) && (has_digit_pair digits))

let () =
  let (min, max) = input in
  let candidates = possible_passwords min max in
  printf "Part 1: %d\n" (List.length candidates);
  let improved_candidates = List.filter ~f:has_only_digit_pair candidates in
  printf "Part 2: %d\n" (List.length improved_candidates);