open! Core

module Outcome = struct
  type t =
    | Loss
    | Draw
    | Win

  let value t =
    match t with
    | Loss -> 0
    | Draw -> 3
    | Win -> 6
  ;;

  let of_xyz c =
    match c with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> raise_s [%message "unexpected input; require XYZ"]
  ;;
end

module Shape = struct
  type t =
    | Rock
    | Paper
    | Scissors

  let of_abc c =
    match c with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> raise_s [%message "unexpected input; require ABC"]
  ;;

  let of_xyz c =
    match c with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> raise_s [%message "unexpected input; require XYZ"]
  ;;

  let value t =
    match t with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  ;;

  let force_outcome t outcome =
    match outcome, t with
    | Outcome.Loss, Paper | Draw, Rock | Win, Scissors -> Rock
    | Loss, Rock | Draw, Scissors | Win, Paper -> Scissors
    | Loss, Scissors | Draw, Paper | Win, Rock -> Paper
  ;;
end

module Round = struct
  type t =
    { opponent : Shape.t
    ; me : Shape.t
    }

  let of_line line =
    let abc, xyz = String.lsplit2_exn line ~on:' ' in
    { opponent = Shape.of_abc abc; me = Shape.of_xyz xyz }
  ;;

  let of_line' line =
    let abc, xyz = String.lsplit2_exn line ~on:' ' in
    let opponent = Shape.of_abc abc in
    let me = xyz |> Outcome.of_xyz |> Shape.force_outcome opponent in
    { opponent; me }
  ;;

  let to_outcome t =
    match t.opponent, t.me with
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Outcome.Loss
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Win
  ;;

  let score t =
    let shape_score = Shape.value t.me in
    let outcome_score = to_outcome t |> Outcome.value in
    shape_score + outcome_score
  ;;
end

let main () =
  let lines = Utils.read_until_eof () in
  let score =
    let rounds = List.map ~f:Round.of_line lines in
    List.sum (module Int) rounds ~f:Round.score
  in
  print_s [%message "Part 1" (score : int)];
  let score =
    let rounds = List.map ~f:Round.of_line' lines in
    List.sum (module Int) rounds ~f:Round.score
  in
  print_s [%message "Part 2" (score : int)]
;;
