open! Core

let read_until_eof () =
  let rec helper acc =
    try
      let line = Caml.read_line () in
      helper (line :: acc)
    with
    | End_of_file -> acc
  in
  helper [] |> List.rev
;;
