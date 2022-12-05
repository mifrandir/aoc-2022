open! Core

let priority_of_item_exn item =
  let ascii = Char.to_int item in
  match ascii > 64, ascii > 96 with
  | false, _ -> raise_s [%message "items should be letters" (item : char)]
  | _, false -> ascii - 64 + 26
  | _, true -> ascii - 96
;;

module Rucksack = struct
  type t =
    { left : Char.Set.t
    ; right : Char.Set.t
    }
  [@@deriving sexp]

  let of_line line =
    let items = String.to_list line in
    let num_items = List.length items in
    { left = List.take items (num_items / 2) |> Char.Set.of_list
    ; right = List.drop items (num_items / 2) |> Char.Set.of_list
    }
  ;;

  let get_shared_items_between x y z =
    let items_x = Set.union x.left x.right in
    let items_y = Set.union y.left y.right in
    let items_z = Set.union z.left z.right in
    let shared_items = items_x |> Set.inter items_y |> Set.inter items_z in
    match Set.length shared_items with
    | 0 ->
      raise_s [%message "rucksacks don't have any shared items" (x : t) (y : t) (z : t)]
    | 1 -> shared_items |> Set.to_list |> List.hd_exn
    | _ -> raise_s [%message "rucksacks share more than one item" (x : t) (y : t) (z : t)]
  ;;

  let get_shared_item_exn t =
    let shared_items = Set.inter t.left t.right in
    match Set.length shared_items with
    | 0 -> raise_s [%message "compartments don't have any shared items" (t : t)]
    | 1 -> shared_items |> Set.to_list |> List.hd_exn
    | _ -> raise_s [%message "compartments share more than one item" (t : t)]
  ;;
end

let run () =
  let rucksacks = Utils.read_until_eof () |> List.map ~f:Rucksack.of_line in
  let sum_of_shared_priorities_by_compartment =
    rucksacks
    |> List.map ~f:Rucksack.get_shared_item_exn
    |> List.sum (module Int) ~f:priority_of_item_exn
  in
  print_s [%message "Part 1" (sum_of_shared_priorities_by_compartment : int)];
  let sum_of_shared_priorities_by_group =
    rucksacks
    |> List.chunks_of ~length:3
    |> List.map ~f:(fun xyz ->
         match xyz with
         | [ x; y; z ] -> Rucksack.get_shared_items_between x y z
         | _ -> raise_s [%message "chunks_of failed"])
    |> List.sum (module Int) ~f:priority_of_item_exn
  in
  print_s [%message "Part 2" (sum_of_shared_priorities_by_group : int)]
;;
