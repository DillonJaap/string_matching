[@@@warning "-32"]

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
      let replace =
        distance_brute_force (drop_prefix from_text 1) (drop_prefix to_text 1)
      in
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
          let replace =
            aux cache (drop_prefix from_text 1) (drop_prefix to_text 1)
          in
          (* Memoize values *)
          Hashtbl.set cache ~key:(from_text, drop_prefix to_text 1) ~data:insert;
          Hashtbl.set cache ~key:(drop_prefix from_text 1, to_text) ~data:delete;
          Hashtbl.set
            cache
            ~key:(drop_prefix from_text 1, drop_prefix to_text 1)
            ~data:replace;
          1 + min_int [ insert; delete; replace ])
    in
    match Hashtbl.find cache (from_text, to_text) with
    | Some v -> v
    | None -> find_min_operation ()
  in
  aux cache from_text to_text
;;

let print_matrix matrix =
  Array.iter matrix ~f:(fun a ->
    print_string "| ";
    Array.iter a ~f:(fun b -> printf "%2d |" b);
    print_endline "")
;;

let levenshtein_distance from_text to_text =
  let from_bytes = Bytes.of_string from_text in
  let to_bytes = Bytes.of_string to_text in
  let tabulated_matrix =
    Array.make_matrix
      ~dimx:(Bytes.length from_bytes + 1)
      ~dimy:(Bytes.length to_bytes + 1)
      0
  in
  (* initialize 1..(length) values in first row and column *)
  Array.iteri tabulated_matrix ~f:(fun i _ -> tabulated_matrix.(i).(0) <- i);
  Array.iteri tabulated_matrix.(0) ~f:(fun i _ -> tabulated_matrix.(0).(i) <- i);
  for i = 1 to Array.length tabulated_matrix - 1 do
    for j = 1 to Array.length tabulated_matrix.(0) - 1 do
      (* print_s ([%sexp_of: bytes] from_bytes); *)
      (* print_matrix tabulated_matrix; *)
      (* print_endline ""; *)
      (* printf "i: %d, j: %d\n" i j; *)
      let res =
        match
          Char.( = ) (Bytes.get from_bytes (i - 1)) (Bytes.get to_bytes (j - 1))
        with
        | true -> tabulated_matrix.(i - 1).(j - 1)
        | false ->
          let insert = tabulated_matrix.(i).(j - 1) + 1 in
          let delete = tabulated_matrix.(i - 1).(j) + 1 in
          let replace = tabulated_matrix.(i - 1).(j - 1) + 1 in
          min_int [ insert; delete; replace ]
      in
      tabulated_matrix.(i).(j) <- res
    done
  done;
  tabulated_matrix.(Bytes.length from_bytes).(Bytes.length to_bytes)
;;

let () =
  let from = "doggy okay now this is a big freaking word" in
  let to_ = "froggy ok now is a big word" in
  (* levenshtein_distance from to_ *)
  (* Stdio.printf "Distance Brute Force: %d\n%!" (distance_brute_force from to_); *)
  Stdio.printf "Distance Memoized: %d\n%!" (distance_memoized from to_);
  Stdio.printf "Distance Levenshtein: %d\n%!" (levenshtein_distance from to_)
;;
