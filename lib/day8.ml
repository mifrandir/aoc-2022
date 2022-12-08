open! Core

module VisibilityLine = struct
  type t = bool list

  let combine t other = List.map2_exn t other ~f:( || )
end

module Line = struct
  type t = int list

  let create line = line |> String.to_list |> List.map ~f:Char.to_int

  let get_visibility t =
    List.fold_map t ~init:0 ~f:(fun max_height height ->
      Int.max max_height height, max_height < height)
    |> snd
  ;;
end

module Visibility = struct
  type t = bool list list

  let combine = List.map2_exn ~f:VisibilityLine.combine
end

module Grid = struct
  type t = Line.t list

  let create lines = List.map lines ~f:Line.create
  let get_immediate_visibility t = List.map t ~f:Line.get_visibility

  let get_visibility t =
    let rotate t = t |> List.rev |> List.transpose_exn in
    let init_visibility = List.map t ~f:(List.map ~f:(fun _ -> false)) in
    List.fold
      [ 0; 1; 2; 3 ]
      ~init:(init_visibility, t)
      ~f:(fun (acc_visibility, grid) _ ->
      let visibility = get_immediate_visibility grid in
      rotate (Visibility.combine acc_visibility visibility), rotate grid)
    |> fst
  ;;

  let num_visible t =
    t |> get_visibility |> List.sum (module Int) ~f:(List.count ~f:Fn.id)
  ;;

  let view_lines_of t i j =
    let ith = List.nth_exn t i in
    let jth = List.nth_exn (List.transpose_exn t) j in
    [ List.drop ith j
    ; List.take ith (j + 1) |> List.rev
    ; List.drop jth i
    ; List.take jth (i + 1) |> List.rev
    ]
  ;;

  let scenic_score t i j =
    view_lines_of t i j
    |> List.map ~f:(fun view_line ->
         match view_line with
         | [] -> raise_s [%message "empty view line"]
         | x :: xs ->
           List.fold_until xs ~init:0 ~finish:Fn.id ~f:(fun acc height ->
             match height < x with
             | true -> Continue_or_stop.Continue (acc + 1)
             | false -> Stop (acc + 1)))
    |> List.fold ~init:1 ~f:( * )
  ;;

  let max_scenic_score t =
    List.fold
      (List.range 0 (List.length t))
      ~init:0
      ~f:(fun acc i ->
        Int.max acc (
        List.fold
          (List.range 0 (List.length t))
          ~init:0
          ~f:(fun acc j -> Int.max acc (scenic_score t i j))))
  ;;
end

let run () =
  let grid = Utils.read_until_eof () |> Grid.create in
  let num_visible = Grid.num_visible grid in
  print_s [%message "Part 1" (num_visible : int)];
  let max_scenic_score = Grid.max_scenic_score grid in
  print_s [%message "Part 1" (max_scenic_score : int)]
;;
