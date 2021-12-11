open Core

(* right, this is where I was like "you can't be serious" *)
module IntPair = struct
  module T = struct
    type t = int * int
    let compare = Stdlib.compare
    let sexp_of_t (x, y) : Sexp.t =
      List [Atom (Int.to_string x); Atom (Int.to_string y)]
  end
  include T
  include Comparator.Make(T)
end

(* collect into list of (x,y) coords with asteroids at center *)
let read_input filename = 
  filename
  |> In_channel.read_lines
  |> List.concat_mapi ~f:(fun y line -> 
      line
      |> String.to_list
      |> List.mapi ~f:(fun x ch -> 
          (x, y,  ch)
        )
      |> List.filter ~f:(fun (_, _, ch) -> Char.equal ch '#')
      |> List.map ~f:(fun (x, y, _) -> (x, y))
    )


let clockwise_atan2 dx dy =
  let th = Float.atan2 (Int.to_float dx) (Int.to_float dy) in
  Float.pi -. th


let relative_angle_distance (from_x, from_y) (to_x, to_y) =
  let dx = to_x - from_x in
  let dy = to_y - from_y in
  let r = Float.sqrt (Int.to_float ((dx * dx) + (dy * dy))) in
  let th = clockwise_atan2 dx dy in
  (r, th)


let eq_point (from_x, from_y) (to_x, to_y) =
  from_x = to_x && from_y = to_y


let lines_of_sight from_point asteroids =
  asteroids
  |> List.filter ~f:(fun to_point -> not(eq_point from_point to_point))
  |> List.map ~f:(fun to_point -> 
      let (r, th) = relative_angle_distance from_point to_point in
      (th, (to_point, r))  
    )
  |> List.sort ~compare:(fun (l_th, _) (r_th, _) -> Float.compare l_th r_th)
  |> Map.of_alist_multi (module Float)


let all_lines_of_sight asteroids =
  asteroids
  |> List.map ~f:(fun from_point -> 
      (from_point, lines_of_sight from_point asteroids)
    )

let best_asteroid asteroids =
  asteroids
  |> all_lines_of_sight
  |> List.max_elt ~compare:(fun (_, l_map) (_, r_map) -> (Map.length l_map) - (Map.length r_map))
  |> Option.value_exn

(* asteroids : Map<th, List<(coord, distance)> *)

let visible_asteroids lines_of_sight =
  lines_of_sight
  |> Map.data
  |> List.filter_map ~f:(fun distances -> 
      match distances with
      | [] -> None
      | (coord, _) :: _ -> Some coord
    )

let after_destruction lines_of_sight =
  lines_of_sight
  |> Map.filter_map ~f:(fun distances ->  
      match distances with
      | [] -> None
      | _ :: t -> Some t
    )

let rec ordered_scan lines_of_sight =
  if (Map.is_empty lines_of_sight) then [] else
    let first_pass = visible_asteroids lines_of_sight in
    let rest = after_destruction lines_of_sight in
    List.append first_pass (ordered_scan rest) 

let sort_distances lines_of_sight =
  lines_of_sight
  |> Map.map ~f:(fun distances ->
      distances 
      |> List.sort ~compare:(fun (_, dl) (_, dr) -> Float.compare dl dr))

let () =
  let asteroids = read_input "./day10/input.txt" in
  let (_, lines_of_sight) = best_asteroid asteroids in
  printf "part 1: %d\n" (Map.length lines_of_sight);
  let sorted_lines = sort_distances lines_of_sight in
  let ordered = ordered_scan sorted_lines in
  let (x, y) = List.nth_exn ordered 199 in
  printf "part 2: %d\n" ((x * 100) + y);
