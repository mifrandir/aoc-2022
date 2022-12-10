open! Core

module State = struct
  type t =
    { register : int
    ; num_cycles : int
    }
  [@@deriving sexp]
end

module Instruction = struct
  type t =
    | Noop
    | AddX of int

  let create line =
    match String.is_prefix line ~prefix:"addx" with
    | true -> Scanf.sscanf line "addx %d" (fun d -> AddX d)
    | false -> Noop
  ;;

  let run t state =
    match t with
    | Noop -> [ { state with State.num_cycles = state.State.num_cycles + 1 } ]
    | AddX v ->
      [ { register = state.register + v; num_cycles = state.num_cycles + 2 }
      ; { state with num_cycles = state.num_cycles + 1 }
      ]
  ;;
end

module Program = struct
  type t = Instruction.t list

  let create lines = List.map lines ~f:Instruction.create

  let run t =
    List.fold
      t
      ~init:[ { State.register = 1; num_cycles = 1 } ]
      ~f:(fun acc instruction ->
        let previous_state = List.hd_exn acc in
        let new_states = Instruction.run instruction previous_state in
        new_states @ acc)
    |> List.rev
  ;;

  let sum_strengths_at t ~cycles =
    t
    |> run
    |> List.sum
         (module Int)
         ~f:(fun { State.register; num_cycles } ->
           match Set.mem cycles num_cycles with
           | false -> 0
           | true -> register * num_cycles)
  ;;

  let render t =
    t
    |> run
    |> List.map ~f:(fun state ->
         let col = (state.num_cycles - 1) mod 40 in
         match col >= state.register - 1 && col <= state.register + 1 with
         | true -> '#'
         | false -> '.')
    |> List.chunks_of ~length:40
    |> List.map ~f:String.of_char_list
    |> List.intersperse ~sep:"\n"
    |> List.fold ~init:"" ~f:( ^ )
  ;;
end

let run () =
  let program = Utils.read_until_eof () |> Program.create in
  let sum =
    Program.sum_strengths_at
      program
      ~cycles:(Int.Set.of_list [ 20; 60; 100; 140; 180; 220 ])
  in
  print_s [%message "Part 1" (sum : int)];
  let display = Program.render program in
  Printf.printf "%s\n" display
;;
