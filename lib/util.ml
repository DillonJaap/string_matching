open Core

(* returns the minimum int in a list, if the list is empty return 0 *)
let min_int list =
  match list with
  | [] -> 0
  | _ ->
    List.fold list ~init:Int.max_value ~f:(fun acc cur ->
      if cur < acc then cur else acc)
;;
