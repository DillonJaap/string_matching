open Core
open String_matching.Util

module String_pair = struct
  module T = struct
    type t = string * string [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

let rec distance_brute_force from_text to_text =
  let length, drop_prefix = String.length, String.drop_prefix in
  match length from_text, length to_text with
  | _, 0 -> length from_text (* base case *)
  | 0, _ -> length to_text (* base case *)
  | _, _ ->
    if Char.equal from_text.[0] to_text.[0]
    then distance_brute_force (drop_prefix from_text 1) (drop_prefix to_text 1)
    else (
      let insert = distance_brute_force from_text (drop_prefix to_text 1) in
      let delete = distance_brute_force (drop_prefix from_text 1) to_text in
      let replace = distance_brute_force (drop_prefix from_text 1) (drop_prefix to_text 1) in
      1 + min_int [ insert; delete; replace ])
;;

let distance_memoized from_text to_text =
  let cache = String_pair.Table.create () in
  let rec aux cache from_text to_text =
    let find_min_operation () =
      let length, drop_prefix = String.length, String.drop_prefix in
      match length from_text, length to_text with
      | _, 0 ->
        Hashtbl.set cache ~key:(from_text, to_text) ~data:(length from_text);
        length from_text
      | 0, _ ->
        Hashtbl.set cache ~key:(from_text, to_text) ~data:(length to_text);
        length to_text
      | _, _ ->
        if Char.equal from_text.[0] to_text.[0]
        then aux cache (drop_prefix from_text 1) (drop_prefix to_text 1)
        else (
          (* operations *)
          let insert = aux cache from_text (drop_prefix to_text 1) in
          let delete = aux cache (drop_prefix from_text 1) to_text in
          let replace = aux cache (drop_prefix from_text 1) (drop_prefix to_text 1) in
          (* Memoize values *)
          Hashtbl.set cache ~key:(from_text, drop_prefix to_text 1) ~data:insert;
          Hashtbl.set cache ~key:(drop_prefix from_text 1, to_text) ~data:delete;
          Hashtbl.set cache ~key:(drop_prefix from_text 1, drop_prefix to_text 1) ~data:replace;
          1 + min_int [ insert; delete; replace ])
    in
    match Hashtbl.find cache (from_text, to_text) with
    | Some v -> v
    | None -> find_min_operation ()
  in
  aux cache from_text to_text
;;

let levenshtein_distance from_text to_text = 
let () =
  let from = "cat" in
  let to_ = "what in the world" in
  Stdio.printf "Distance Brute Force: %d\n%!" (distance_brute_force from to_);
  Stdio.printf "Distance Memoized: %d\n%!" (distance_memoized from to_)
;;
