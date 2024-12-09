open Core
open Advent

type file =
  { start : int
  ; size : int
  }

type block =
  | File
  | Free

let construct_fs line =
  let rec mk acc id block data =
    match data, block with
    | n :: hd, File -> mk (Array.append acc (Array.create ~len:n id)) (id + 1) Free hd
    | n :: hd, Free -> mk (Array.append acc (Array.create ~len:n (-1))) id File hd
    | [], _ -> acc
  in
  mk [||] 0 File (String.to_list line |> List.map ~f:Char.get_digit_exn)
;;

let check_fs fs =
  fs
  |> Array.mapi ~f:(fun i num ->
    match num with
    | -1 -> 0
    | n -> i * n)
  |> Array.to_list
  |> List.fold ~init:0 ~f:(fun acc n -> acc + n)
;;

let find_file max fs i =
  let rec find_last_non_free i =
    if i < 0 || fs.(i) <> -1 then i else find_last_non_free (i - 1)
  in
  let rec find_file_size s i =
    if i < 0 || fs.(i) <> fs.(s) || s - i >= max then i else find_file_size s (i - 1)
  in
  let last_non_free = find_last_non_free (i - 1) in
  if last_non_free < 0
  then None
  else (
    let e = last_non_free in
    let start = find_file_size e e in
    Some { start = start + 1; size = e - start })
;;

let rec is_free fs i size =
  if size = 0
  then true
  else if i >= Array.length fs || fs.(i) <> -1
  then false
  else is_free fs (i + 1) (size - 1)
;;

let rec find_free fs size index =
  if index >= Array.length fs
  then None
  else if is_free fs index size
  then Some index
  else find_free fs size (index + 1)
;;

let rec move fs file i offset =
  if offset < file.size
  then (
    fs.(i + offset) <- fs.(file.start + offset);
    fs.(file.start + offset) <- -1;
    move fs file i (offset + 1))
;;

let compress_fs max fs =
  let rec aux fs j =
    match find_file max fs j with
    | None -> ()
    | Some f ->
      (match find_free fs f.size 0 with
       | None -> aux fs f.start
       | Some i ->
         if i < f.start then move fs f i 0;
         aux fs f.start;
         ())
  in
  aux fs (Array.length fs)
;;

let () =
  let line = read_line_as_one "./inputs/d9input.txt" in
  let fs1 = construct_fs line in
  compress_fs 1 fs1;
  (*answer:6310675819476*)
  let res = check_fs fs1 in
  let fs2 = construct_fs line in
  compress_fs 10 fs2;
  (*answer:6335972980679*)
  let res2 = check_fs fs2 in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
