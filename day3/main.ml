open Core

let manhattan_distance_from_origin (x,y) = abs(x) + abs(y)

type direction = Up | Down | Left | Right

exception UnknownDirection of char

let uncons_string str =
  let hd = String.get str 0 in
  let tl = String.sub str ~pos:1 ~len:((String.length str) - 1) in
  (hd, tl)

let parse_entry str =
  let (hd, tl) = uncons_string str in
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
  let paths = filename 
  |> In_channel.read_lines
  |> List.map ~f:parse_path in
  let left = List.nth_exn paths 0 in
  let right = List.nth_exn paths 1 in
  (left, right)

exception NoOriginProvided

let rec points_on_vector state (x, y) (dir, magnitude) =
  if magnitude = 0 then (x, y) :: state else
  let point = match dir with
  | Up -> (x, y - 1)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y) in
  points_on_vector (point :: state) point (dir, magnitude - 1)

let points_in_path path =
  path 
  |> List.fold 
    ~init:[(0, 0)] 
    ~f:(fun points vector -> 
        match points with
        | (origin :: rest) -> points_on_vector rest origin vector
        | _ -> raise NoOriginProvided) 
  |> List.rev

(* yiiiiikes *)
let encode (x, y) = String.concat ~sep:"," [Int.to_string x; Int.to_string y] 
let decode value = 
  let parts = String.split_on_chars ~on:[','] value in
  let x = List.nth_exn parts 0 in
  let y = List.nth_exn parts 1 in  
  (Int.of_string x, Int.of_string y)
let set_of list =
  Set.of_list (module String) (List.map ~f:encode list)

let intersect_list l r =
(Set.inter (set_of l) (set_of r))
  |> Set.to_list
  |> List.map ~f:decode

exception NotFound
let rec find_index list item i =
  let (x1, y1) = item in
  match list with
  | [] -> raise NotFound
  | ((x0, y0) :: t) -> if (x0 = x1) && (y0 = y1) then i else (find_index t item (i + 1))

let print_point (x, y) = 
  printf "(%d,%d)" x y

let print_point_list points =
  let _ = List.map ~f:print_point points in
  printf "\n"

let total_wire_length point left_points right_points =
  let left_index = find_index left_points point 0 in
  let right_index = find_index right_points point 0 in
  left_index + right_index + 2

type point_with_metadata = {
  point: (int * int);
  distance_from_origin: int;
  total_wire_length: int;
}

let with_metadata point left_points right_points = 
  { 
    point = point; 
    distance_from_origin = manhattan_distance_from_origin point;  
    total_wire_length = total_wire_length point left_points right_points;
  }

let () =
  let (left, right) = parse_file "./day3/input.txt" in
  let left_points =  points_in_path left in
  let right_points = points_in_path right in
  let intersections = intersect_list left_points right_points in
  let intersections_with_metadata = List.map ~f:(fun point -> with_metadata point left_points right_points) intersections in
  let sorted_by_distance = List.sort ~compare:(fun l r -> l.distance_from_origin - r.distance_from_origin) intersections_with_metadata in
  let { distance_from_origin; _ } = List.nth_exn sorted_by_distance 0 in
  printf "part 1: %d\n" distance_from_origin;
  let sorted_by_total_length = List.sort ~compare:(fun l r -> l.total_wire_length - r.total_wire_length) intersections_with_metadata in
  let { total_wire_length; _ } = List.nth_exn sorted_by_total_length 0 in
  printf "part2: %d\n" total_wire_length;