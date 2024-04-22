open Core
module IntSet = Set.Make (Int)

type scratchcard =
  { id : int
  ; winning : IntSet.t
  ; picked : IntSet.t
  }

let drop_prefix line =
  let prefix_pattern = Re.compile (Re.Perl.re "Card\\s+\\d+:") in
  Re.replace ~all:false prefix_pattern line ~f:(fun _ -> "")
;;

let to_scratchcard col line =
  let to_int_set str =
    String.split str ~on:' '
    |> List.filter_map ~f:(fun s ->
      if String.is_empty s then None else Some (Int.of_string s))
    |> IntSet.of_list
  in
  let split_lists = drop_prefix line |> String.split ~on:'|' in
  match split_lists with
  | [ winning; picked ] ->
    { id = col + 1; winning = to_int_set winning; picked = to_int_set picked }
  | _ -> invalid_arg "Given input is malformed"
;;

let count_wins { winning; picked; _ } = Set.inter winning picked |> Set.length

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
