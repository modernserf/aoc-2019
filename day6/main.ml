open Core

let read_input filename = 
  filename
  |> In_channel.read_lines
  |> List.map ~f:(fun x -> 
      let parts = String.split_on_chars ~on:[')'] x in
      let parent = List.nth_exn parts 0 in
      let child = List.nth_exn parts 1 in
      (child, parent)
    )
  |> Map.of_alist_exn (module String)

let rec parent_chain ?(path=[]) map child =
  match Map.find map child with
  | Some parent -> parent_chain ~path:(child :: path) map parent
  | None -> path
  
let map_sum fn list = 
  List.fold ~init:0 ~f:(fun l r -> l + (fn r)) list

let count_orbits child_parent_map =
  child_parent_map
  |> Map.keys
  |> map_sum (fun child ->  List.length (parent_chain child_parent_map child))

let rec common_ancestors ?(ancestors=[]) list_a list_b =
  match (list_a, list_b) with
  | (ha :: ta, hb :: tb) when (String.equal ha hb) -> common_ancestors ~ancestors:(ha :: ancestors) ta tb
  | _ -> (ancestors, list_a, list_b)

let tree_distance child_parent_map a b =
  let a_chain = parent_chain child_parent_map a in
  let b_chain = parent_chain child_parent_map b in
  let (_, a_to_common, b_to_common) = common_ancestors a_chain b_chain in
  (List.length a_to_common) + (List.length b_to_common) - 2

let () =
  let map = read_input "./day6/input.txt" in
  let orbits = count_orbits map in
  printf "part 1: %d\n" orbits;
  let transfers = tree_distance map "YOU" "SAN" in
  printf "part 2: %d\n" transfers;
