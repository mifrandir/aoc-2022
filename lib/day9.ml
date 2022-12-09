open! Core

module Step = struct
  type t =
    | L
    | R
    | D
    | U
  [@@deriving sexp]

  let create c =
    match c with
    | 'L' -> L
    | 'R' -> R
    | 'D' -> D
    | 'U' -> U
    | _ -> raise_s [%message "unknown direction" (c : char)]
  ;;
end

let sign x =
  match Int.sign x with
  | Pos -> 1
  | Zero -> 0
  | Neg -> -1
;;

module Knot = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp]

  let start = { x = 0; y = 0 }

  let move t step =
    match step with
    | Step.L -> { t with x = t.x - 1 }
    | R -> { t with x = t.x + 1 }
    | D -> { t with y = t.y - 1 }
    | U -> { t with y = t.y + 1 }
  ;;

  let follow first second =
    let dx = second.x - first.x in
    let dy = second.y - first.y in
    match Int.abs dx > 1 || Int.abs dy > 1 with
    | false -> second
    | true -> { x = second.x - sign dx; y = second.y - sign dy }
  ;;
end

module Rope = struct
  type t = Knot.t list [@@deriving sexp]

  let create length = List.map (List.range 0 length) ~f:(fun _ -> Knot.start)

  let apply t step =
    match t with
    | [] -> raise_s [%message "empty rope"]
    | head :: tail ->
      let head = Knot.move head step in
      head
      :: List.folding_map tail ~init:head ~f:(fun previous knot ->
           let knot = Knot.follow previous knot in
           knot, knot)
  ;;
end

module Sequence = struct
  type t = Step.t list [@@deriving sexp]

  let create lines =
    List.map lines ~f:(fun line ->
      Scanf.sscanf line "%c %d" (fun c d ->
        List.range 0 d |> List.map ~f:(fun _ -> Step.create c)))
    |> List.join
  ;;

  let num_visited_by_tail t n =
    let rope_states =
      List.fold t ~init:[ Rope.create n ] ~f:(fun acc step ->
        let rope = Rope.apply (List.hd_exn acc) step in
        rope :: acc)
    in
    let num_states = List.length rope_states in
    List.map rope_states ~f:(fun rope->
      let tail_x = (List.last_exn rope).Knot.x in
      let tail_y = (List.last_exn rope).Knot.y in
      (tail_x * 2 * num_states) + tail_y)
    |> Int.Set.of_list
    |> Set.length
  ;;
end

let run () =
  let sequence = Utils.read_until_eof () |> Sequence.create in
  let num_visited_by_tail = Sequence.num_visited_by_tail sequence 2 in
  print_s [%message "Part 1" (num_visited_by_tail : int)];
  let num_visited_by_tail = Sequence.num_visited_by_tail sequence 10 in
  print_s [%message "Part 2" (num_visited_by_tail : int)]
;;
