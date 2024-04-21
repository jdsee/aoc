open Core

module Int_tuple = struct
  module T = struct
    type t = int * int

    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let equal = Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal
  end

  include T
  include Comparator.Make (T)
end

let _merge_adjacent_with lines ~f =
  List.fold lines ~init:[] ~f:(fun acc line ->
    let xs = f line in
    match acc with
    | [] -> [ xs; xs ]
    | curr :: prev :: tl -> xs :: (xs @ curr) :: (xs @ prev) :: tl
    | _ -> [])
  |> List.drop_last
  |> Option.value ~default:[]
  |> List.rev
;;
