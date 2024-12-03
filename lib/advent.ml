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
