open! Core

(* Let's just say I'm not particularly proud of this one... *)

module Move = struct
  type t =
    { count : int
    ; source : int
    ; destination : int
    }

  let of_line line =
    Scanf.sscanf line "move %d from %d to %d" (fun count source destination ->
      { count; source = source - 1; destination = destination - 1 })
  ;;
end

module Arrangement = struct
  type t = char list list [@@deriving sexp]

  let of_lines lines =
    lines
    |> List.map ~f:(fun line -> line |> String.to_list |> List.chunks_of ~length:4)
    |> List.transpose_exn
    |> List.map ~f:(fun stack -> stack |> List.map ~f:(fun cell -> List.nth_exn cell 1))
    |> List.map ~f:(List.filter ~f:(fun c -> not (Char.equal c ' ')))
  ;;

  let move_one t source destination =
    assert (source <> destination);
    let crate, remaining =
      let stack = List.nth_exn t source in
      assert (List.length stack > 0);
      List.hd_exn stack, List.tl_exn stack
    in
    List.mapi t ~f:(fun i stack ->
      match i = source, i = destination with
      | true, _ -> remaining
      | _, true -> crate :: stack
      | _, _ -> stack)
  ;;

  let rec move_n_individually t source destination n =
    match n with
    | 0 -> t
    | _ -> move_n_individually (move_one t source destination) source destination (n - 1)
  ;;

  let move_n t source destination n =
    assert (source <> destination);
    let moving, remaining =
      let stack = List.nth_exn t source in
      assert (List.length stack >= n);
      List.take stack n, List.drop stack n
    in
    List.mapi t ~f:(fun i stack ->
      match i = source, i = destination with
      | true, _ -> remaining
      | _, true -> List.append moving stack
      | _, _ -> stack)
  ;;

  let apply ?(part2 = false) t (move : Move.t) =
    match part2 with
    | false -> move_n_individually t move.source move.destination move.count
    | true -> move_n t move.source move.destination move.count
  ;;
end

let run () =
  let arrangement_lines, move_lines =
    let lines = Utils.read_until_eof () in
    let empty_line_index, _ =
      List.findi lines ~f:(fun _ s -> String.length s = 0) |> Option.value_exn
    in
    List.take lines (empty_line_index - 1), List.drop lines (empty_line_index + 1)
  in
  let arrangement = Arrangement.of_lines arrangement_lines in
  let moves = List.map move_lines ~f:Move.of_line in
  let crates_at_top =
    let arrangement_after_moves =
      List.fold moves ~init:arrangement ~f:(fun acc move -> Arrangement.apply acc move)
    in
    List.map arrangement_after_moves ~f:List.hd_exn |> String.of_char_list
  in
  print_s [%message "Part 1" (crates_at_top : string)];
  let crates_at_top =
    let arrangement_after_moves =
      List.fold moves ~init:arrangement ~f:(fun acc move ->
        Arrangement.apply ~part2:true acc move)
    in
    List.map arrangement_after_moves ~f:List.hd_exn |> String.of_char_list
  in
  print_s [%message "Part 2" (crates_at_top : string)]
;;
