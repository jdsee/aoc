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

(* TODO: Figure out how to make f generic in it's output *)
let zip_next_with xs ~f =
  List.fold xs ~init:([], None) ~f:(fun (acc, prev) curr ->
    match prev with
    | Some prev -> f curr prev :: acc, None
    | None -> acc, Some curr)
  |> Tuple2.map_fst ~f:List.rev
;;

let zip_next = zip_next_with ~f:(fun a b -> b, a)

let intersect_intervals (a_start, a_end) (b_start, b_end) =
  if a_start > b_end || b_start > a_end
  then None
  else Some (min a_start b_start, min a_start b_start)
;;
