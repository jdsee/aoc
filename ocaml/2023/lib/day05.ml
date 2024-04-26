open Core
open Aoc_util

type range_mapping =
  { dest_start : int
  ; src_start : int
  ; length : int
  }

type src_dest_map =
  { src : string
  ; dest : string
  ; map : range_mapping list
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
    let* length = wdigit in
    return { dest_start; src_start; length }
  ;;

  let source_destination_map =
    let* src = alpha <* string "-to-" in
    let* dest = alpha <* string " map:" <* whitespace in
    let* map = sep_by1 whitespace range_map in
    return { src; dest; map }
  ;;

  let almanac =
    let* seeds = string "seeds: " *> sep_by1 whitespace digit <* whitespace in
    let* src_dest_maps = sep_by1 whitespace source_destination_map in
    return (seeds, src_dest_maps)
  ;;

  let parse_almanac = parse_string ~consume:Prefix almanac
end

let lookup seed (src_dest_map : src_dest_map) =
  let is_in_range { src_start; length; _ } =
    seed >= src_start && seed <= src_start + length
  in
  match List.find src_dest_map.map ~f:is_in_range with
  | Some { src_start; dest_start; _ } -> dest_start + (seed - src_start)
  | None -> seed
;;

let find_closest_loc seeds (src_dest_maps : src_dest_map list) =
  let find_loc seed = List.fold src_dest_maps ~init:seed ~f:lookup in
  List.map seeds ~f:find_loc |> List.min_elt ~compare:Int.compare |> Option.value_exn
;;

let with_almanac f input =
  match In_channel.read_all input |> Parser.parse_almanac with
  | Ok (seeds, src_dest_maps) -> f seeds src_dest_maps
  | Error err -> Fmt.invalid_arg "@.Failed to parse almanac: %s" err
;;

let solve_part_one = with_almanac find_closest_loc

let lookup_range { src_start; dest_start; length = map_len } (k_start, k_len) =
  let k_end = k_start + k_len - 1 in
  let src_end = src_start + map_len - 1 in
  if k_start > src_end || src_start > k_end
  then []
  else (
    let inter_start = Int.max k_start src_start in
    let inter_end = Int.min k_end src_end in
    let inter_len = inter_end - inter_start + 1 in
    let offset = inter_start - src_start in
    let k' = dest_start + offset, inter_len in
    let prefix =
      if k_start < src_start then Some (k_start, src_start - k_start) else None
    in
    let suffix =
      if k_end > src_end then Some (inter_end + 1, k_end - src_end) else None
    in
    Fmt.pr "@.@.k_start: %d" k_start;
    Fmt.pr "@.k_len: %d" k_len;
    Fmt.pr "@.k_end: %d" k_end;
    Fmt.pr "@.src_start: %d" src_start;
    Fmt.pr "@.src_len: %d" map_len;
    Fmt.pr "@.src_end: %d" src_end;
    Fmt.pr "@.dest_start: %d; map_len: %d" dest_start map_len;
    Fmt.pr "@.inter_start: %d" inter_start;
    Fmt.pr "@.inter_end: %d" inter_end;
    Fmt.pr "@.inter_len: %d" inter_len;
    Fmt.pr "@.offset: %d" inter_start;
    Tuple2.uncurry (Fmt.pr "@.k': %d %d") k';
    Tuple2.uncurry (Fmt.pr "@.prefix: %d %d") (Option.value prefix ~default:(-1, -1));
    Tuple2.uncurry (Fmt.pr "@.suffix: %d %d@.@.") (Option.value suffix ~default:(-1, -1));
    k' :: List.filter_opt [ prefix; suffix ])
;;

let map_ranges (maps : src_dest_map list) seed_ranges =
  List.fold maps ~init:seed_ranges ~f:(fun ks map ->
    Fmt.pr "@.>>> Mapping over %s-%s map <<<@." map.src map.dest;
    List.concat_map ks ~f:(fun ((k_start, k_len) as k) ->
      Fmt.pr "@.Lookup (%d, %d)" k_start k_len;
      List.fold_until
        map.map
        ~init:k
        ~finish:(fun ((start, len) as k') ->
          Fmt.pr "@.Nothing found, moving on with (%d, %d)@.@." start len;
          [ k' ])
        ~f:(fun k ({ src_start; dest_start; length; _ } as mapping) ->
          Fmt.pr "@. .. in (%d, %d, %d)" src_start dest_start length;
          match lookup_range mapping k with
          | [] -> Continue k
          | ks' ->
            Fmt.pr "@.-> Found ranges:";
            List.iter ks' ~f:(fun (start, len) -> Fmt.pr "@.- (%d, %d)" start len);
            Fmt.pr "@.@.";
            Stop ks')))
;;

let find_closest_loc_by_range seeds all_maps =
  let seed_ranges = Util.zip_next seeds |> Tuple2.get1 in
  let loc_ranges = map_ranges all_maps seed_ranges in
  Fmt.pr "@.@.Found the follwing location ranges:";
  List.map loc_ranges ~f:(fun (start, len) ->
    Fmt.pr "@.- (%d, %d)" start len;
    start)
  |> List.min_elt ~compare:Int.compare
  |>
  (Fmt.pr "@.@.";
   Option.value_exn)
;;

let solve_part_two = with_almanac find_closest_loc_by_range
