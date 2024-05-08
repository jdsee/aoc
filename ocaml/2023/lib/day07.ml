open Core

type hand_type =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
[@@deriving ord, show]

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
  [@@deriving eq, ord, show]

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
end

module Hand = struct
  type t =
    { hand_type : hand_type
    ; ranks : Rank.t list
    ; bid : int
    }
  [@@deriving show]

  let compare t1 t2 =
    let rec compare_ranks t1 t2 =
      match t1, t2 with
      | t1_hd :: t1_tl, t2_hd :: t2_tl ->
        let comp = Rank.compare t1_hd t2_hd in
        if comp <> 0 then comp else compare_ranks t1_tl t2_tl
      | _ -> 0
    in
    let type_comp = compare_hand_type t1.hand_type t2.hand_type in
    if type_comp <> 0 then type_comp else compare_ranks t1.ranks t2.ranks
  ;;
end

let eval (hand, bid) ~jacks_are_jokers : Hand.t =
  let ranks = List.map hand ~f:(Rank.of_char ~jacks_are_jokers) in
  let jokers, other =
    List.partition_tf ranks ~f:(Rank.equal Joker) |> Tuple2.map_fst ~f:List.length
  in
  let rank_lengths =
    List.sort_and_group ~compare:Rank.compare other
    |> List.map ~f:List.length
    |> List.sort ~compare:(Fn.flip Int.compare)
  in
  let hand_type =
    match jokers, rank_lengths with
    | 0, [ 5 ] -> FiveOfAKind
    | 1, [ 4 ] -> FiveOfAKind
    | 2, [ 3 ] -> FiveOfAKind
    | 3, [ 2 ] -> FiveOfAKind
    | 4, [ 1 ] -> FiveOfAKind
    | 5, [] -> FiveOfAKind
    | 0, 4 :: _ -> FourOfAKind
    | 1, 3 :: _ -> FourOfAKind
    | 2, 2 :: _ -> FourOfAKind
    | 3, 1 :: _ -> FourOfAKind
    | 0, [ 3; 2 ] -> FullHouse
    | 1, [ 2; 2 ] -> FullHouse
    | 0, 3 :: _ -> ThreeOfAKind
    | 1, 2 :: _ -> ThreeOfAKind
    | 2, 1 :: _ -> ThreeOfAKind
    | 0, 2 :: 2 :: _ -> TwoPair
    | 0, 2 :: _ -> OnePair
    | 1, 1 :: _ -> OnePair
    | _, _ -> HighCard
  in
  { hand_type; ranks; bid }
;;

let parse_hand line =
  match String.split ~on:' ' line with
  | [ hand; bid ] -> hand |> String.to_list, bid |> Int.of_string
  | _ -> failwith "Invalid input"
;;

let solve input ~jacks_are_jokers =
  In_channel.read_lines input
  |> List.map ~f:(fun line -> parse_hand line |> eval ~jacks_are_jokers)
  |> List.sort ~compare:(Fn.flip Hand.compare)
  |> List.foldi ~init:0 ~f:(fun i acc { Hand.bid; _ } -> acc + ((i + 1) * bid))
;;

let solve_part_one = solve ~jacks_are_jokers:false
let solve_part_two = solve ~jacks_are_jokers:true
