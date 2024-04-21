open Aoc

let () =
  Fmt.pr "[ DAY 03 ]\nPart I:\n";
  Day03.solve_part_one "inputs/day_03.test" |> Fmt.pr "  Test - %d\n";
  Day03.solve_part_one "inputs/day_03" |> Fmt.pr "  Main - %d\n";
  Fmt.pr "Part II:\n";
  Day03.solve_part_two "inputs/day_03.test" |> Fmt.pr "  Test - %d\n";
  Day03.solve_part_two "inputs/day_03" |> Fmt.pr "  Main - %d\n"
;;
