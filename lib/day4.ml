open! Core

module Pair = struct
  module Assignment = struct
    type t =
      { first : int
      ; last : int
      }

    let of_string_exn str =
      let left, right = String.lsplit2_exn str ~on:'-' in
      { first = int_of_string left; last = int_of_string right }
    ;;

    let contains_all_of t other = t.first <= other.first && t.last >= other.last
    let strictly_smaller t other = t.first < other.first && t.last < other.first
  end

  type t =
    { first : Assignment.t
    ; second : Assignment.t
    }

  let of_line line =
    let left, right = String.lsplit2_exn line ~on:',' in
    { first = Assignment.of_string_exn left; second = Assignment.of_string_exn right }
  ;;

  let is_fully_overlapping t =
    Assignment.contains_all_of t.first t.second
    || Assignment.contains_all_of t.second t.first
  ;;

  let is_disjoint t =
    Assignment.strictly_smaller t.first t.second
    || Assignment.strictly_smaller t.second t.first
  ;;

  let is_overlapping t = not (is_disjoint t)
end

let run () =
  let pairs = Utils.read_until_eof () |> List.map ~f:Pair.of_line in
  let num_fully_overlapping =
    pairs |> List.filter ~f:Pair.is_fully_overlapping |> List.length
  in
  print_s [%message "Part 1" (num_fully_overlapping : int)];
  let num_overlapping = pairs |> List.filter ~f:Pair.is_overlapping |> List.length in
  print_s [%message "Part 2" (num_overlapping : int)]
;;
