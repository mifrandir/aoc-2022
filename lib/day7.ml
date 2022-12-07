open! Core

module File = struct
  type t =
    { name : string
    ; size : int
    }
  [@@deriving sexp]

  let create line = Scanf.sscanf line "%d %s" (fun size name -> { name; size })
end

module Command = struct
  type t =
    | Cd of string
    | CdUp
    | Ls of { output : File.t list }

  let get_files input =
    let rec helper acc input =
      match input with
      | [] -> acc, input
      | next :: remaining ->
        (match String.get next 0, String.is_prefix next ~prefix:"dir" with
         | '$', _ -> acc, input
         | _, true -> helper acc remaining
         | _, false ->
           let file = File.create next in
           helper (file :: acc) remaining)
    in
    helper [] input
  ;;

  let parse input =
    match input with
    | [] -> raise_s [%message "no command to parse"]
    | next :: input_after_next ->
      (match next with
       | "$ cd .." -> CdUp, input_after_next
       | "$ ls" ->
         let files, input_after_files = get_files input_after_next in
         Ls { output = files }, input_after_files
       | other -> Scanf.sscanf other "$ cd %s" (fun name -> Cd name, input_after_next))
  ;;

  let parse_all input =
    let rec helper acc input =
      match input with
      | [] -> acc
      | other ->
        let command, remaining = parse other in
        helper (command :: acc) remaining
    in
    helper [] input |> List.rev
  ;;
end

module Directory = struct
  type t =
    { name : string
    ; dirs : t list
    ; files : File.t list
    ; size : int
    }
  [@@deriving sexp]

  let create name dirs files =
    let file_size = List.sum (module Int) files ~f:(fun file -> file.File.size) in
    let dir_size = List.sum (module Int) dirs ~f:(fun dir -> dir.size) in
    { name; dirs; files; size = file_size + dir_size }
  ;;

  let rec parse name commands =
    let rec step dirs files commands =
      match commands with
      | [] -> create name dirs files, commands
      | next :: remaining ->
        (match next with
         | Command.CdUp -> create name dirs files, remaining
         | Command.Cd name ->
           let child, commands_after_child = parse name remaining in
           step (child :: dirs) files commands_after_child
         | Command.Ls { output } -> step dirs (output @ files) remaining)
    in
    step [] [] commands
  ;;

  let rec sum_size_less_than_k t ~k =
    let from_children = List.sum (module Int) t.dirs ~f:(sum_size_less_than_k ~k) in
    match t.size <= k with
    | true -> from_children + t.size
    | false -> from_children
  ;;

  let rec smallest_larger_than_k t ~k =
    match t.size >= k with
    | false -> Int.max_value
    | true ->
      List.fold t.dirs ~init:t.size ~f:(fun acc dir ->
        Int.min acc (smallest_larger_than_k dir ~k))
  ;;
end

let run () =
  let commands = Utils.read_until_eof () |> Command.parse_all |> List.tl_exn in
  let root, _ = Directory.parse "/" commands in
  print_s [%message (root : Directory.t)];
  let sum = Directory.sum_size_less_than_k root ~k:100000 in
  print_s [%message "Part 1" (sum : int)];
  let smallest_larger_than_k =
    let unused_space = 70000000 - root.size in
    Directory.smallest_larger_than_k root ~k:(30000000 - unused_space)
  in
  print_s [%message "Part 2" (smallest_larger_than_k : int)]
;;
