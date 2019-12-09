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
