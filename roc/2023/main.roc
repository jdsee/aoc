app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }

import pf.Stdout
import pf.Task
import "input" as mainInput : Str
import "input.test" as testInput : Str

Sort implements
    compare : a, a -> [EQ, GT, LT] where a implements Sort

sortAsc = \xs -> List.sortWith xs \a, b -> compare b a

Ranks : List Rank

Rank := [Ace, King, Queen, Jack, Ten, Nine, Eigth, Seven, Six, Five, Four, Three, Two, Joker]
    implements [
        Eq,
        Hash,
        Inspect,
        Sort { compare: compareRank },
    ]

compareRank = \a, b -> Num.compare (rankToOrd a) (rankToOrd b)

HandType := [FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard]
    implements [
        Eq,
        Hash,
        Inspect,
        Sort { compare: compareHandType },
    ]

compareHandType = \a, b -> Num.compare (handTypeToOrd a) (handTypeToOrd b)

Hand := { type : HandType, ranks : Ranks, bid : U64 }
    implements [
        Eq,
        Hash,
        Inspect,
        Sort { compare: compareHand },
    ]

compareHand = \@Hand a, @Hand b ->
    when compareHandType a.type b.type is
        EQ -> compareRanks a.ranks b.ranks
        unequal -> unequal

compareRanks = \l1, l2 ->
    when (l1, l2) is
        ([], []) -> EQ
        ([_, ..], []) -> GT
        ([], [_, ..]) -> LT
        ([a, ..], [b, ..]) ->
            when compare a b is
                EQ -> compareRanks (List.dropFirst l1 1) (List.dropFirst l2 1)
                unequal -> unequal

rankToOrd = \@Rank rank ->
    when rank is
        Ace -> 0
        King -> 1
        Queen -> 2
        Jack -> 3
        Ten -> 4
        Nine -> 5
        Eigth -> 6
        Seven -> 7
        Six -> 8
        Five -> 9
        Four -> 10
        Three -> 11
        Two -> 12
        Joker -> 13

handTypeToOrd = \@HandType handType ->
    when handType is
        FiveOfAKind -> 0
        FourOfAKind -> 1
        FullHouse -> 2
        ThreeOfAKind -> 3
        TwoPair -> 4
        OnePair -> 5
        HighCard -> 6

parseHand = \line, withJokers ->
    when Str.split line " " is
        [left, right] ->
            ranks = parseRanks left withJokers
            type <- eval ranks |> Result.try
            bid <- Str.toU64 right |> Result.map
            @Hand { type, ranks, bid }

        _ -> Err InvalidInput

parseRanks = \s, withJokers ->
    Str.toUtf8 s |> List.keepOks \c -> toRank c withJokers

toRank = \c, withJokers ->
    when c is
        'A' -> Ok (@Rank Ace)
        'K' -> Ok (@Rank King)
        'Q' -> Ok (@Rank Queen)
        'J' -> Ok (if withJokers then @Rank Joker else @Rank Jack)
        'T' -> Ok (@Rank Ten)
        '9' -> Ok (@Rank Nine)
        '8' -> Ok (@Rank Eigth)
        '7' -> Ok (@Rank Seven)
        '6' -> Ok (@Rank Six)
        '5' -> Ok (@Rank Five)
        '4' -> Ok (@Rank Four)
        '3' -> Ok (@Rank Three)
        '2' -> Ok (@Rank Two)
        _ -> Err (InvalidInput c)

eval : Ranks -> Result HandType [InvalidInput]
eval = \hand ->
    counts = groupByCount hand
    jokers = Dict.get counts (@Rank Joker) |> Result.withDefault 0
    others = Dict.remove counts (@Rank Joker) |> Dict.values |> List.sortDesc
    when others is
        [] if jokers == 5 -> Ok (@HandType FiveOfAKind)
        [hd, ..] if hd + jokers == 5 -> Ok (@HandType FiveOfAKind)
        [hd, ..] if hd + jokers == 4 -> Ok (@HandType FourOfAKind)
        [3, 2] -> Ok (@HandType FullHouse)
        [2, 2] if jokers == 1 -> Ok (@HandType FullHouse)
        [hd, ..] if hd + jokers == 3 -> Ok (@HandType ThreeOfAKind)
        [2, 2, ..] -> Ok (@HandType TwoPair)
        [2, ..] -> Ok (@HandType OnePair)
        [1, ..] if jokers == 1 -> Ok (@HandType OnePair)
        [1, 1, 1, 1, 1] -> Ok (@HandType HighCard)
        _ -> Err InvalidInput

groupByCount = \xs ->
    List.walk xs (Dict.empty {}) \acc, k ->
        Dict.update acc k \entry ->
            when entry is
                Missing -> Present 1
                Present count -> Present (count + 1)

solve = \input, withJokers ->
    input
    |> Str.split "\n"
    |> List.keepOks \line -> parseHand line withJokers
    |> sortAsc
    |> List.walkWithIndex 0 (\acc, @Hand { bid }, i -> acc + (i + 1) * bid)

main =
    Stdout.line! "-- Part one --"
    Stdout.line! (Str.concat "Test:" (solve testInput Bool.false |> Num.toStr))
    Stdout.line! (Str.concat "Main:" (solve mainInput Bool.false |> Num.toStr))
    Stdout.line! "-- Part two --"
    Stdout.line! (Str.concat "Test:" (solve testInput Bool.true |> Num.toStr))
    Stdout.line! (Str.concat "Main:" (solve mainInput Bool.true |> Num.toStr))

expect eval [@Rank Ace, @Rank Two, @Rank Ace, @Rank Joker, @Rank Queen] == Ok (@HandType ThreeOfAKind)

# Ranks should get sorted properly
expect
    res = sortAsc [@Rank Five, @Rank Ace, @Rank Joker]
    res == [@Rank Joker, @Rank Five, @Rank Ace]

# Hand types should get sorted properly
expect
    res = sortAsc [@HandType FullHouse, @HandType HighCard, @HandType FourOfAKind]
    res == [@HandType HighCard, @HandType FullHouse, @HandType FourOfAKind]

# Hands should get sorted properly
expect
    hand1 = @Hand {
        type: @HandType TwoPair,
        ranks: [@Rank Joker, @Rank Ace, @Rank Three, @Rank Three, @Rank Nine],
        bid: 42,
    }
    hand2 = @Hand {
        type: @HandType TwoPair,
        ranks: [@Rank Joker, @Rank Ace, @Rank Two, @Rank Three, @Rank Nine],
        bid: 42,
    }
    res = sortAsc [hand1, hand2]
    res == [hand2, hand1]

