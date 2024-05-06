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

type rank =
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
[@@deriving ord, show]

let rank_of_char = function
  | 'A' -> Ace
  | 'K' -> King
  | 'Q' -> Queen
  | 'J' -> Jack
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

module Hand = struct
  type t =
    { hand_type : hand_type
    ; ranks : rank list
    ; bid : int
    }
  [@@deriving show]

  let compare t1 t2 =
    let rec compare_ranks t1 t2 =
      match t1, t2 with
      | t1_hd :: t1_tl, t2_hd :: t2_tl ->
        let comp = compare_rank t1_hd t2_hd in
        if comp <> 0 then comp else compare_ranks t1_tl t2_tl
      | _ -> 0
    in
    let type_comp = compare_hand_type t1.hand_type t2.hand_type in
    if type_comp <> 0 then type_comp else compare_ranks t1.ranks t2.ranks
  ;;
end

let eval (hand, bid) : Hand.t =
  let rank_lengths =
    List.sort_and_group ~compare:Char.compare hand
    |> List.map ~f:List.length
    |> List.sort ~compare:(Fn.flip Int.compare)
  in
  let hand_type =
    match rank_lengths with
    | [ 5 ] -> FiveOfAKind
    | [ 4; _ ] -> FourOfAKind
    | [ 3; 2 ] -> FullHouse
    | 3 :: _ -> ThreeOfAKind
    | 2 :: 2 :: _ -> TwoPair
    | 2 :: _ -> OnePair
    | _ -> HighCard
  in
  let ranks = List.map hand ~f:rank_of_char in
  { hand_type; ranks; bid }
;;

let parse_hand line =
  match String.split ~on:' ' line with
  | [ hand; bid ] -> hand |> String.to_list, bid |> Int.of_string
  | _ -> failwith "Invalid input"
;;

let solve_part_one input =
  In_channel.read_lines input
  |> List.map ~f:(Fn.compose eval parse_hand)
  |> List.sort ~compare:(Fn.flip Hand.compare)
  |> List.foldi ~init:0 ~f:(fun i acc { Hand.bid; _ } -> acc + ((i + 1) * bid))
;;
