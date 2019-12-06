open Core


let read_input filename = 
  filename
  |> In_channel.read_lines
  |> Lines.map ~f:(fun x -> 
      let parts = String.split_on_chars ~on:[','] x in
        let parent = List.nth_exn parts 0 in
        let child = List.nth_exn parts 1 in
        (child, parent)
    )
  |> Map.of_alist_exn (module String)


let rec count_parent_chain map child sum =
  match Map.find map child with
  | Some parent -> count_parent_chain map parent (sum + 1)
  | None -> sum
  
let count_orbits child_parent_map =
  Map.fold 
    ~init:0 
    ~f:(fun (child, _) sum ->
      count_parent_chain child_parent_map child sum
    )
    child_parent_map

let () =
  let map = read_input "./day6/input.txt" in
  let orbits = count_orbits map in
  printf "part 1: %d\n" orbits;
   