open Core

(* note: integer division truncates the result *)
let fuel_required mass =
  (mass / 3) - 2

let rec fuel_required_iter mass state =
  let fuel_mass = fuel_required mass in
  if fuel_mass <= 0 then state else fuel_required_iter fuel_mass (state + fuel_mass)

let fuel_required_recursive mass = fuel_required_iter mass 0

let map_sum fn list = 
  List.fold ~init:0 ~f:(fun l r -> l + (fn r)) list

let () =
  let masses = List.map ~f:Int.of_string (In_channel.read_lines "./day1/input1.txt") in 
  printf "part 1: %d\n" (map_sum fuel_required masses);
  printf "part 2: %d\n" (map_sum fuel_required_recursive masses)