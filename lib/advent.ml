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

let print_int_triple triple =
  List.iter ~f:(fun (x, y, z) -> Printf.printf "(%d, %d, %d)\n" x y z) triple
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

let construct_char_grid (s : string list) =
  List.map s ~f:(fun s -> Stdlib.String.to_seq s |> Stdlib.Array.of_seq) |> Array.of_list
;;

let find_char_coordinates search_char grid =
  let find_row row_index row =
    let row_length = String.length row in
    let rec find_col col_index acc =
      if col_index >= row_length
      then acc
      else (
        let acc =
          if Char.equal row.[col_index] search_char
          then (row_index, col_index) :: acc
          else acc
        in
        find_col (col_index + 1) acc)
    in
    find_col 0 []
  in
  let rec find_in_grid row_index rows acc =
    match rows with
    | [] -> List.rev acc
    | row :: rest ->
      let row_coords = find_row row_index row in
      find_in_grid (row_index + 1) rest (row_coords @ acc)
  in
  find_in_grid 0 grid []
;;

let print_2d_array arr =
  Array.iter
    ~f:(fun row ->
      Array.iter ~f:(Printf.printf "%c ") row;
      (* Print each character in the row *)
      Printf.printf "\n" (* Newline after each row *))
    arr
;;

let char_grid_to_string_grid charr =
  Array.to_list
    (Array.map charr ~f:(fun arr -> String.init (Array.length arr) ~f:(Array.get arr)))
;;

module IntTripleComparator = struct
  module T = struct
    type t = int * int * int

    let compare (a1, b1, c1) (a2, b2, c2) =
      match Int.compare a1 a2 with
      | 0 ->
        (match Int.compare b1 b2 with
         | 0 -> Int.compare c1 c2
         | other -> other)
      | other -> other
    ;;

    let sexp_of_t (a, b, c) =
      Sexp.List [ Int.sexp_of_t a; Int.sexp_of_t b; Int.sexp_of_t c ]
    ;;

    let t_of_sexp = function
      | Sexp.List [ a; b; c ] -> Int.t_of_sexp a, Int.t_of_sexp b, Int.t_of_sexp c
      | _ -> failwith "Invalid S-expression for int * int * int"
    ;;
  end

  include T
  include Comparator.Make (T)
end

let concat_integers a b =
  let rec count_digits n = if n = 0 then 0 else 1 + count_digits (n / 10) in
  let num_digits_b = count_digits b in
  (a * Int.pow 10 num_digits_b) + b
;;
