open Core

let read_input filename = 
  filename
  |> In_channel.read_all
  |> String.to_list
  |> List.map ~f:(fun char -> char |> String.of_char |> Int.of_string)

let to_layers input width height =
  input
  |> List.chunks_of ~length:width
  |> List.chunks_of ~length:height

let map_sum fn list = 
  List.fold ~init:0 ~f:(fun l r -> l + (fn r)) list

let layer_digit_count match_digit layer =
  map_sum (fun row -> row |> List.filter ~f:(fun digit -> digit = match_digit) |> List.length) layer

let layer_with_min_zeroes layers =
  layers
  |> List.fold 
    ~init:(Int.max_value, []) 
    ~f:(fun (count, prev) next -> 
      let next_count = layer_digit_count 0 next in
      if next_count < count then (next_count, next) else (count, prev)
    )
  |> (fun (_ , layer) -> layer)

let zip_deep over_layer under_layer =
  List.zip_exn over_layer under_layer
  |>  List.map ~f:(fun (over_row, under_row) -> List.zip_exn over_row under_row)

let map_deep ~f layer =
  List.map ~f:(List.map ~f:f) layer

let merge_cell (over, under) =
  match over with
  | 2 -> under
  | _ -> over

let merge_layers over_layer under_layer =
  zip_deep over_layer under_layer 
  |> map_deep ~f:merge_cell

let merge_all_layers layers =
  List.reduce_exn ~f:(merge_layers) layers

let print_cell cell = 
  (* printf "%d" cell *)
  match cell with
  | 0 -> printf " "
  | _ -> printf "#"

let print_row row =
  let _ = List.map ~f:print_cell row in
  printf "\n"

let print_layer layer =
  let _ = List.map ~f:print_row layer in
  ()

let () =
  let input = read_input "./day8/input.txt" in
  let (width, height) = (25, 6) in
  let layers = to_layers input width height in
  let layer = layer_with_min_zeroes layers in
  let product = (layer_digit_count 1 layer) * (layer_digit_count 2 layer) in
  printf "Part 1: %d\n" product;
  let merged = merge_all_layers layers in
  printf "Part 2:\n";
  print_layer merged;
