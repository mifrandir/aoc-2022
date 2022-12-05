open! Core

let find_max calories = List.fold calories ~init:0 ~f:Int.max

(* Supreme O(nlgn) implementation. *)
let find_max_k k calories =
  let sorted = calories |> List.sort ~compare:Int.compare |> List.rev in
  let max_k = List.take sorted k in
  List.sum (module Int) ~f:Fn.id max_k
;;

let calories_per_elf (lines : string list) =
  lines
  |> List.group ~break:(fun _ delim -> String.equal delim "")
  |> List.map
       ~f:
         (List.sum
            (module Int)
            ~f:(fun x ->
              match x with
              | "" -> 0
              | number -> int_of_string number))
;;

let run () =
  let calories =
    let input = Utils.read_until_eof () in
    calories_per_elf input
  in
  let max_calories = find_max calories in
  print_s [%message "Part 1" (max_calories : int)];
  let max_3_calories = find_max_k 3 calories in
  print_s [%message "Part 2" (max_3_calories : int)]
;;
