open Core
open Angstrom

type range_mapping =
  { dest_start : int
  ; src_start : int
  ; length : int
  }

module Parser = struct
  let spaces = many1 @@ char ' '
  let whitespace = take_while1 Char.is_whitespace
  let digit = take_while1 Char.is_digit >>| Int.of_string <?> "Expected a digit"
  let numbers = sep_by1 spaces digit <?> "Expected at least one number"
  let alpha = take_while Char.is_alpha

  let range_mapping =
    let to_range_mapping = function
      | [ dest_start; src_start; length ] -> { dest_start; src_start; length }
      | _ -> failwith "Unexpected amount of numbers in source-destination-map"
    in
    numbers >>| to_range_mapping
  ;;

  let source_destination_map =
    let* _source = alpha <* string "-to-" in
    let* _destination = alpha <* string " map:" <* whitespace in
    sep_by1 whitespace range_mapping
  ;;

  let almanac =
    let* seeds = string "seeds: " *> sep_by1 whitespace digit <* whitespace in
    let* src_dest_maps = sep_by1 whitespace source_destination_map in
    return (seeds, src_dest_maps)
  ;;

  let parse_almanac = parse_string ~consume:Prefix almanac
end

let lookup seed range_mappings =
  let is_in_range { src_start; length; _ } =
    seed >= src_start && seed <= src_start + length
  in
  match List.find range_mappings ~f:is_in_range with
  | Some { src_start; dest_start; _ } -> dest_start + (seed - src_start)
  | None -> seed
;;

let find_closest_location seeds src_dest_maps =
  let find_location seed = List.fold src_dest_maps ~init:seed ~f:lookup in
  List.map seeds ~f:find_location |> List.min_elt ~compare:Int.compare |> Option.value_exn
;;

let solve_part_one input =
  match In_channel.read_all input |> Parser.parse_almanac with
  | Ok (seeds, src_dest_maps) -> find_closest_location seeds src_dest_maps
  | Error err -> Fmt.invalid_arg "@.Failed to parse almanac: %s" err
;;

let solve_part_two _input = 42
