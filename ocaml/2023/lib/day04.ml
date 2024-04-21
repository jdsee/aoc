open Core

type scratchcard =
  { id : int
  ; winning : int list
  ; picked : int list
  }

let drop_prefix line =
  let prefix_pattern = Re.compile (Re.Perl.re "Card\\s+\\d+:") in
  Re.replace ~all:false prefix_pattern line ~f:(fun _ -> "")
;;

let to_scratchcard col line =
  let to_int_list str =
    String.split str ~on:' '
    |> List.filter_map ~f:(function
      | "" -> None
      | x -> Int.of_string_opt x)
  in
  let split_lists = drop_prefix line |> String.split ~on:'|' in
  match split_lists with
  | [ winning; picked ] ->
    { id = col + 1; winning = to_int_list winning; picked = to_int_list picked }
  | _ -> invalid_arg "Given input is malformed"
;;

let count_wins { winning; picked; _ } =
  List.count picked ~f:(List.mem winning ~equal:Int.equal)
;;

let solve_part_one input =
  In_channel.read_lines input
  |> List.mapi ~f:(fun col line ->
    let wins = to_scratchcard col line |> count_wins in
    if wins = 0 then 0 else Int.pow 2 (wins - 1))
  |> List.fold ~f:( + ) ~init:0
;;

let score_scratchcards all_wins =
  let rec loop rem_wins n =
    match n, rem_wins with
    | 0, _ | _, [] -> 0
    | n, curr_wins :: rest -> 1 + loop rest curr_wins + loop rest (n - 1)
  in
  loop all_wins (List.length all_wins)
;;

let solve_part_two input =
  In_channel.read_lines input
  |> List.mapi ~f:(fun col line -> to_scratchcard col line |> count_wins)
  |> score_scratchcards
;;
