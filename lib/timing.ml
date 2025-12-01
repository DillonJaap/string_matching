open Core

let time_function f x =
  let start_time = Time_now.nanoseconds_since_unix_epoch () in
  let result = f x in
  let end_time = Time_now.nanoseconds_since_unix_epoch () in
  let duration = Int63.((end_time - start_time) / of_int 1_000_000) in
  Printf.printf "Execution took %s ms\n" (Int63.to_string duration);
  result
;;
