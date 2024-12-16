open Core
open Advent

let count_xmas grid =
  let search = "XMAS" in
  let rows = List.length grid in
  let cols = String.length (List.hd_exn grid) in
  let rec word_exists row col row_offset col_offset search_point =
    if search_point = 4
    then true
    else if row < 0 || col < 0 || row >= rows || col >= cols
    then false
    else if not (Char.equal (List.nth_exn grid row).[col] search.[search_point])
    then false
    else
      word_exists
        (row + row_offset)
        (col + col_offset)
        row_offset
        col_offset
        (search_point + 1)
  in
  let count_from_position row col =
    let offsets = [ 0, 1; 0, -1; 1, 1; 1, -1; 1, 0; -1, 1; -1, -1; -1, 0 ] in
    List.fold offsets ~init:0 ~f:(fun acc (row_offset, col_offset) ->
      if word_exists row col row_offset col_offset 0 then acc + 1 else acc)
  in
  let rec traverse row col acc =
    if row >= rows
    then acc
    else if col >= cols
    then traverse (row + 1) 0 acc
    else traverse row (col + 1) (acc + count_from_position row col)
  in
  traverse 0 0 0
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

let count_mas_cross centers mcord scord =
  let offsets =
    [ (-1, 1), (1, -1); (1, 1), (-1, -1); (-1, -1), (1, 1); (1, -1), (-1, 1) ]
  in
  List.fold centers ~init:0 ~f:(fun acc ccord ->
    if
      List.fold offsets ~init:0 ~f:(fun acc2 (moffset, soffset) ->
        if
          List.mem mcord (add_pairs moffset ccord) ~equal:pairs_equal
          && List.mem scord (add_pairs soffset ccord) ~equal:pairs_equal
        then acc2 + 1
        else acc2)
      = 2
    then acc + 1
    else acc)
;;

let () =
  let lines = read_lines "./inputs/d4/input.txt" in
  (*answer:2496*)
  let res = count_xmas lines in
  let mcord = find_char_coordinates 'M' lines in
  let acord = find_char_coordinates 'A' lines in
  let scord = find_char_coordinates 'S' lines in
  (*answer:1967*)
  let res2 = count_mas_cross acord mcord scord in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
