open Core

let extract_nums line =
  let second l = List.nth l 1 |> Option.value_exn in
  String.split ~on:':' line
  |> second
  |> String.strip
  |> String.split ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let int_tuple_of_string a b = Int.of_string a, Int.of_string b

let read_time_distance_pairs input =
  let vals = In_channel.read_lines input |> List.map ~f:extract_nums in
  match vals with
  | [ time; distance ] -> List.map2_exn ~f:int_tuple_of_string time distance
  | _ -> invalid_arg "Invalid format of given input"
;;

let winner_times_of time distance =
  let rec find i = if i * (time - i) > distance then i else find (i + 1) in
  let offset = find (distance / time) in
  time - (2 * offset) + 1
;;

let solve_part_one input =
  read_time_distance_pairs input
  |> List.map ~f:(Tuple2.uncurry winner_times_of)
  |> List.reduce ~f:( * )
  |> Option.value_exn
;;

let extract_num line =
  let scnd l = List.nth l 1 in
  String.split ~on:':' line
  |> scnd
  |> Option.value_exn
  |> String.substr_replace_all ~pattern:" " ~with_:""
  |> Int.of_string
;;

let solve_part_two input =
  let nums = In_channel.read_lines input |> List.map ~f:extract_num in
  match nums with
  | [ time; distance ] -> winner_times_of time distance
  | _ -> invalid_arg "Invalid format of given input"
;;
