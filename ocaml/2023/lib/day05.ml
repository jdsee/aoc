open Core
open Aoc_util

type range_mapping =
  { dest_start : int
  ; src_start : int
  ; map_len : int
  }

module Parser = struct
  open Angstrom

  let spaces = many1 @@ char ' ' <?> "Expected at least one space"
  let whitespace = take_while Char.is_whitespace
  let digit = take_while1 Char.is_digit >>| Int.of_string <?> "Expected a digit"
  let wdigit = whitespace *> digit <* whitespace
  let numbers = sep_by1 spaces digit <?> "Expected at least one number"
  let alpha = take_while Char.is_alpha
  let skip_line = take_till (Char.equal '\n') <* whitespace

  let range_map =
    let* dest_start = wdigit in
    let* src_start = wdigit in
    let* map_len = wdigit in
    return { dest_start; src_start; map_len }
  ;;

  let src_dest_map = skip_line *> sep_by1 whitespace range_map

  let almanac =
    let* seeds = string "seeds: " *> sep_by1 whitespace digit <* whitespace in
    let* src_dest_maps = sep_by1 whitespace src_dest_map in
    return (seeds, src_dest_maps)
  ;;

  let parse_almanac = parse_string ~consume:Prefix almanac
end

let lookup seed src_dest_map =
  let is_in_range { src_start; map_len; _ } =
    seed >= src_start && seed <= src_start + map_len
  in
  match List.find src_dest_map ~f:is_in_range with
  | Some { src_start; dest_start; _ } -> dest_start + (seed - src_start)
  | None -> seed
;;

let find_closest_loc seeds src_dest_maps =
  let find_loc seed = List.fold src_dest_maps ~init:seed ~f:lookup in
  List.map seeds ~f:find_loc |> List.min_elt ~compare:Int.compare |> Option.value_exn
;;

let with_almanac f input =
  match In_channel.read_all input |> Parser.parse_almanac with
  | Ok (seeds, src_dest_maps) -> f seeds src_dest_maps
  | Error err -> Fmt.invalid_arg "@.Failed to parse almanac: %s" err
;;

let solve_part_one = with_almanac find_closest_loc

type lookup_result =
  { intersection : (int * int) option
  ; unresolved : (int * int) list
  }

let lookup_range { src_start; dest_start; map_len } ((k_start, k_len) as k) =
  let k_end = k_start + k_len in
  let src_end = src_start + map_len in
  if k_start > src_end || src_start > k_end
  then { intersection = None; unresolved = [ k ] }
  else (
    let inter_start = Int.max k_start src_start in
    let inter_end = Int.min k_end src_end in
    let inter_len = inter_end - inter_start in
    let offset = inter_start - src_start in
    let k' = dest_start + offset, inter_len in
    let prefix =
      if k_start < src_start then Some (k_start, src_start - k_start) else None
    in
    let suffix = if k_end > src_end then Some (inter_end, k_end - src_end) else None in
    { intersection = Some k'; unresolved = List.filter_opt [ prefix; suffix ] })
;;

let rec translate_ranges ks map =
  match map with
  | [] -> ks
  | mapping :: tl ->
    List.concat_map ks ~f:(fun k ->
      match lookup_range mapping k with
      | { intersection = Some k; unresolved = [] } -> [ k ]
      | { intersection = Some k; unresolved } -> k :: translate_ranges unresolved tl
      | { intersection = None; unresolved } -> translate_ranges unresolved tl)
;;

let find_closest_loc_by_range seeds maps =
  let seed_ranges = Util.zip_next seeds |> Tuple2.get1 in
  List.fold maps ~init:seed_ranges ~f:translate_ranges
  |> List.map ~f:Tuple2.get1
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

let solve_part_two = with_almanac find_closest_loc_by_range
