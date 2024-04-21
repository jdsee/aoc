open Core
open Aoc_util

let is_symbol = function
  | '$' | '%' | '*' | '/' | '@' | '=' | '+' | '-' | '#' | '&' -> true
  | _ -> false
;;

let indices_where row line ~f =
  String.to_list line
  |> List.filter_mapi ~f:(fun col c -> if f c then Some (row, col) else None)
;;

let asterisk_indices = List.concat_mapi ~f:(indices_where ~f:(Char.equal '*'))
let symbol_indices = List.concat_mapi ~f:(indices_where ~f:is_symbol)

let collect_nums_indexed line =
  String.to_list line
  |> List.foldi ~init:[] ~f:(fun col acc c ->
    match c, acc with
    | '0' .. '9', (start, num) :: tl when start + String.length num = col ->
      (start, num ^ String.of_char c) :: tl
    | '0' .. '9', _ -> (col, String.of_char c) :: acc
    | _ -> acc)
;;

let find_adjacent_nums ref_pos row start num =
  let stop = start + String.length num in
  List.range (start - 1) (stop + 1)
  |> List.concat_map ~f:(fun col -> [ row - 1, col; row, col; row + 1, col ])
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
