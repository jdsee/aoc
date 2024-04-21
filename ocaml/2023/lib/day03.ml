open Core
open Aoc_util

let is_symbol = function
  | '$' | '%' | '*' | '/' | '@' | '=' | '+' | '-' | '#' | '&' -> true
  | _ -> false
;;

let indices_where line_nr line ~f =
  String.to_list line
  |> List.filter_mapi ~f:(fun i c -> if f c then Some (line_nr, i) else None)
;;

let asterisk_indices = List.concat_mapi ~f:(indices_where ~f:(Char.equal '*'))
let symbol_indices = List.concat_mapi ~f:(indices_where ~f:is_symbol)

let collect_nums_indexed line =
  let keep_digits i c = if Char.is_digit c then Some (i, c) else None in
  String.to_list line
  |> List.filter_mapi ~f:keep_digits
  |> List.fold ~init:[] ~f:(fun acc (i, digit) ->
    match acc with
    | (start, num) :: tl when start + String.length num = i ->
      (start, num ^ String.of_char digit) :: tl
    | _ -> (i, String.of_char digit) :: acc)
;;

let find_adjacent_nums ref_pos line_nr start num =
  let stop = start + String.length num in
  List.range (start - 1) (stop + 1)
  |> List.concat_map ~f:(fun i -> [ line_nr - 1, i; line_nr, i; line_nr + 1, i ])
  |> List.filter_map ~f:(fun pos ->
    if Set.mem ref_pos pos then Some (pos, int_of_string num) else None)
;;

let collect_adjacent_nums ref_pos line_nr line =
  collect_nums_indexed line
  |> List.concat_map ~f:(Tuple2.uncurry (find_adjacent_nums ref_pos line_nr))
;;

let solve_part_one input =
  let lines = In_channel.read_lines input in
  let ref_pos = symbol_indices lines |> Set.of_list (module Util.Int_tuple) in
  List.concat_mapi lines ~f:(collect_adjacent_nums ref_pos)
  |> List.fold ~init:0 ~f:(fun acc (_, num) -> acc + num)
;;

let solve_part_two input =
  let lines = In_channel.read_lines input in
  let ref_pos = asterisk_indices lines |> Set.of_list (module Util.Int_tuple) in
  List.concat_mapi lines ~f:(collect_adjacent_nums ref_pos)
  |> List.Assoc.sort_and_group ~compare:Util.Int_tuple.compare
  |> List.fold ~init:0 ~f:(fun acc (_, matches) ->
    match matches with
    | [ a; b ] -> acc + (a * b)
    | _ -> acc + 0)
;;
