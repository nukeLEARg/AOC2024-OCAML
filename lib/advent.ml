open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun chan ->
    let x = In_channel.input_all chan in
    String.split_lines x)
;;

let split_first s =
  let a = String.split ~on:' ' s in
  List.hd_exn a
;;

let square x = x *. x
let num_filter = Re.Pcre.re {|\d+|} |> Re.compile

let print_string_list lst =
  let () = Printf.printf "\n[" in
  List.iter ~f:(Printf.printf "\"%s\", ") lst;
  Printf.printf "]\n"
;;

let print_int_list lst =
  Printf.printf "\n[";
  List.iter ~f:(Printf.printf "%d; ") lst;
  Printf.printf "]\n"
;;

let print_int_pairs pairs =
  List.iter ~f:(fun (x, y) -> Printf.printf "(%d, %d)\n" x y) pairs
;;

let add_pairs (a1, b1) (a2, b2) = a1 + a2, b1 + b2
let pairs_equal (a1, b1) (a2, b2) = a1 = a2 && b1 = b2

let remove_at_index lst index =
  let rec aux i = function
    | [] -> [] (* If the list is empty, return the empty list *)
    | _x :: xs when i = index ->
      xs (* If the index matches, return the tail of the list *)
    | x :: xs -> x :: aux (i + 1) xs (* Otherwise, keep the element and recurse *)
  in
  aux 0 lst (* Start the recursion with index 0 *)
;;

let rec split_on_empty before lst =
  match lst with
  | [] -> before, []
  | "" :: rest -> List.rev before, rest
  | x :: rest -> split_on_empty (x :: before) rest
;;
