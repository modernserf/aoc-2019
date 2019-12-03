open Core

let manhattan_distance (x1,y1) (x2,y2) =
  abs(x1-x2) + abs(y1-y2)


type direction = Up | Down | Left | Right

exception UnknownDirection of char

let parse_entry (hd :: tl) =
  let magnitude = Int.of_string tl in
  let dir = match hd with 
  | 'U' -> Up
  | 'D' -> Down
  | 'L' -> Left
  | 'R' -> Right
  | _ -> raise (UnknownDirection hd) in
  (dir, magnitude)

let parse_path path_str =
  path_str
  |> String.split_on_chars ~on:[',']
  |> List.map ~f:parse_entry

let parse_file filename = 
  let [left, right] = filename
  |> In_channel.read_lines
  |> List.map ~f:parse_path in
  (left, right)

let points_on_vector state (x, y) (dir, magnitude) =
  if magnitude = 0 then state else
  let point = match dir with
  | Up -> (x, y - 1)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y) in
  points_on_vector (point :: state) (x, y) (dir, magnitude - 1)

let points_in_path path origin =
  List.fold 
    ~init:[origin] 
    ~f:(fun (origin::rest) vector -> points_on_vector rest origin vector) 
    path

let find_closest_crossing (left, right) =
  let left_points = Set.of_list (points_in_path left (0, 0)) in
  let right_points = Set.of_list (points_in_path right (0, 0)) in
  let intersections = Set.to_list (Set.inter left_points right_points) in
  let with_distance = List.map ~f:(fun point -> (point, manhattan_distance point (0, 0))) intersections in
  let (_ :: (_, distance) :: _) = List.sort ~compare:(fun (_, distance) -> distance) with_distance in
  distance

let () =
  let paths = parse_file "./day3/input.txt" in
  let distance = find_closest_crossing paths in
  println "part 1: %d" distance;