open! Core

module Transmission = struct
  type t = char Array.t

  let of_line line = line |> String.to_list_rev |> List.rev |> Array.of_list

  let get_last_k t i k =
    List.range ~start:`exclusive ~stop:`inclusive (i - k) i
    |> List.filter ~f:(fun j -> j >= 0 && j < Array.length t)
    |> List.map ~f:(Array.get t)
    |> Char.Set.of_list
  ;;

  let has_eop_marker_at t i =
    let relevant = get_last_k t i 4 in
    Set.length relevant = 4
  ;;

  let has_som_marker_at t i =
    let relevant = get_last_k t i 14 in
    Set.length relevant = 14
  ;;
end

let run () =
  match Utils.read_until_eof () with
  | [] -> raise_s [%message "Empty input"]
  | line :: _ ->
    let transmission = Transmission.of_line line in
    let end_of_packet =
      let end_of_packet =
        List.range 0 (Array.length transmission)
        |> List.find_exn ~f:(fun i -> Transmission.has_eop_marker_at transmission i)
      in
      end_of_packet + 1
    in
    print_s [%message "Part 1" (end_of_packet : int)];
    let start_of_message =
      let start_of_message =
        List.range 0 (Array.length transmission)
        |> List.find_exn ~f:(fun i -> Transmission.has_som_marker_at transmission i)
      in
      start_of_message + 1
    in
    print_s [%message "Part 2" (start_of_message : int)]
;;
