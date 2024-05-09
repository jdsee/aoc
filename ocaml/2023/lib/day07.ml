open Core
open Aoc_util

type hand_type =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
[@@deriving ord, show, sexp]

module Rank = struct
  type t =
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eigth
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    | Joker
  [@@deriving eq, ord, show, sexp]

  let of_char ?(jacks_are_jokers = false) = function
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> if jacks_are_jokers then Joker else Jack
    | 'T' -> Ten
    | '9' -> Nine
    | '8' -> Eigth
    | '7' -> Seven
    | '6' -> Six
    | '5' -> Five
    | '4' -> Four
    | '3' -> Three
    | '2' -> Two
    | _ -> failwith "Unknown rank"
  ;;

  let parse str ~jacks_are_jokers =
    String.to_list str |> List.map ~f:(of_char ~jacks_are_jokers)
  ;;
end

module Hand = struct
  type t =
    { hand_type : hand_type
    ; ranks : Rank.t list
    ; bid : int
    }
  [@@deriving show, sexp]

  let compare t1 t2 =
    let rec compare_ranks t1 t2 =
      match t1, t2 with
      | [], [] -> 0
      | _ :: _, [] -> 1
      | [], _ :: _ -> -1
      | hd1 :: tl1, hd2 :: tl2 ->
        (match Rank.compare hd1 hd2 with
         | 0 -> compare_ranks tl1 tl2
         | other -> other)
    in
    match compare_hand_type t1.hand_type t2.hand_type with
    | 0 -> compare_ranks t1.ranks t2.ranks
    | other -> other
  ;;
end

module RankMap = Map.Make (Rank)

let eval ranks =
  let sortDesc = List.sort ~compare:(Fn.flip Int.compare) in
  let counts = Util.groupByCount ~init:RankMap.empty ranks in
  let jokers = Map.find counts Joker |> Option.value ~default:0 in
  let others = Map.remove counts Joker |> Map.data |> sortDesc in
  match others with
  | [] when jokers = 5 -> FiveOfAKind
  | hd :: _ when hd + jokers = 5 -> FiveOfAKind
  | hd :: _ when hd + jokers = 4 -> FourOfAKind
  | [ 3; 2 ] -> FullHouse
  | [ 2; 2 ] when jokers = 1 -> FullHouse
  | hd :: _ when hd + jokers = 3 -> ThreeOfAKind
  | [ 2; 2; _ ] -> TwoPair
  | 2 :: _ -> OnePair
  | 1 :: _ when jokers = 1 -> OnePair
  | [ 1; 1; 1; 1; 1 ] -> HighCard
  | _ -> failwith "invalid input"
;;

let parse_hand line ~jacks_are_jokers : Hand.t =
  match String.split ~on:' ' line with
  | [ left; right ] ->
    let ranks = left |> Rank.parse ~jacks_are_jokers in
    let hand_type = eval ranks in
    let bid = right |> Int.of_string in
    { hand_type; ranks; bid }
  | _ -> failwith "Invalid input"
;;

let solve input ~jacks_are_jokers =
  In_channel.read_lines input
  |> List.map ~f:(parse_hand ~jacks_are_jokers)
  |> List.sort ~compare:(Fn.flip Hand.compare)
  |> List.foldi ~init:0 ~f:(fun i acc { Hand.bid; _ } -> acc + ((i + 1) * bid))
;;

let solve_part_one = solve ~jacks_are_jokers:false
let solve_part_two = solve ~jacks_are_jokers:true
