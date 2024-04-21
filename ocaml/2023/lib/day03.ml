open Core
open Aoc_util

let is_symbol c =
  let symbols = [ '$'; '%'; '*'; '/'; '@'; '='; '+'; '-'; '#'; '&' ] in
  List.mem symbols c ~equal:Char.equal
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let merge_adjacent_with xs ~f =
  let prev = [] :: (List.drop_last xs |> Option.value ~default:[]) in
  let succ = List.drop xs 1 @ [ [] ] in
  List.map3_exn prev xs succ ~f
;;

let concat_adjacent =
  let concat_uniq a b c = List.dedup_and_sort ~compare (a @ b @ c) in
  merge_adjacent_with ~f:concat_uniq
;;

let adjacent_indices_where str ~f =
  let with_neighbors x = [ x - 1; x; x + 1 ] in
  String.to_list str
  |> List.concat_mapi ~f:(fun i c -> if f c then with_neighbors i else [])
;;

let collect_digits allowed line =
  let keep_digits i c = if is_digit c then Some (i, c) else None in
  String.to_list line
  |> List.filter_mapi ~f:keep_digits
  |> List.fold ~init:[] ~f:(fun acc (i, digit) ->
    match acc with
    | (start, stop, num) :: tl when stop = i - 1 ->
      (start, i, num ^ String.of_char digit) :: tl
    | _ -> (i, i, String.of_char digit) :: acc)
  |> List.filter_map ~f:(fun (start, stop, num) ->
    let range = List.range start stop ~stop:`inclusive in
    let is_adjacent = List.exists range ~f:(List.mem allowed ~equal:Int.equal) in
    Option.some_if is_adjacent (Int.of_string num))
  |> List.fold ~f:( + ) ~init:0
;;

let input_data = In_channel.read_lines "inputs/day_03.test"

let indices_where line_nr line ~f =
  String.to_list line
  |> List.filter_mapi ~f:(fun i c -> if f c then Some (line_nr, i) else None)
;;

let asterisk_indices = List.concat_mapi ~f:(indices_where ~f:(Char.equal '*'))

let collect_nums_indexed line =
  let keep_digits i c = if is_digit c then Some (i, c) else None in
  String.to_list line
  |> List.filter_mapi ~f:keep_digits
  |> List.fold ~init:[] ~f:(fun acc (i, digit) ->
    match acc with
    | (start, num) :: tl when start + String.length num = i ->
      (start, num ^ String.of_char digit) :: tl
    | _ -> (i, String.of_char digit) :: acc)
;;

let adjacent_indices ref_pos line_nr start num =
  let stop = start + String.length num in
  List.range (start - 1) (stop + 1)
  |> List.concat_map ~f:(fun i -> [ line_nr - 1, i; line_nr, i; line_nr + 1, i ])
  |> List.filter_map ~f:(fun pos ->
    if Set.mem ref_pos pos then Some (pos, int_of_string num) else None)
;;

let collect_gears ref_pos line_nr line =
  collect_nums_indexed line
  |> List.concat_map ~f:(Tuple2.uncurry (adjacent_indices ref_pos line_nr))
;;

let solve_part_one input =
  let lines = In_channel.read_lines input in
  let symbol_indices = adjacent_indices_where ~f:is_symbol in
  let adjacent_indices = List.map lines ~f:symbol_indices |> concat_adjacent in
  List.map2_exn adjacent_indices lines ~f:collect_digits |> List.fold ~init:0 ~f:( + )
;;

let solve_part_two input =
  let lines = In_channel.read_lines input in
  let ref_pos = asterisk_indices lines |> Set.of_list (module Util.Int_tuple) in
  List.concat_mapi lines ~f:(collect_gears ref_pos)
  |> List.Assoc.sort_and_group ~compare:Util.Int_tuple.compare
  |> List.fold ~init:0 ~f:(fun acc (_, matches) ->
    match matches with
    | [ a; b ] -> acc + (a * b)
    | _ -> acc + 0)
;;
