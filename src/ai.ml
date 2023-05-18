open Goalcheck

let total_elements arr = Array.length arr * Array.length arr

let array_to_list_1D arr : string list =
  let size_a = Array.length arr in
  let return_lst = ref [] in
  for i = size_a - 1 downto 0 do
    return_lst := arr.(i) :: !return_lst
  done;
  !return_lst

let array_to_list_2D arr =
  let size_a = size arr in
  let return_lst = ref [] in
  for i = size_a - 1 downto 0 do
    for j = size_a - 1 downto 0 do
      return_lst := arr.(i).(j) :: !return_lst
    done
  done;
  !return_lst

let rec list_to_dict lst index =
  match lst with
  | [] -> []
  | h :: t -> (index, h) :: list_to_dict t (index + 1)

let rec rem_spots_helper d =
  match d with
  | [] -> []
  | (k, v) :: t ->
      if v <> "O" && v <> "X" then k :: rem_spots_helper t
      else rem_spots_helper t

let rem_spots arr =
  let list_rep = array_to_list_2D arr in
  let dict_rep = list_to_dict list_rep 0 in
  let remaining_spots = rem_spots_helper dict_rep in
  remaining_spots

let available arr inp : bool =
  let remaining_spots = rem_spots arr in
  if List.mem inp remaining_spots then true else false

(*dumb ai*)
let rec rand_index curr_arr =
  let n1 = Random.int (total_elements curr_arr - 1) in
  if available curr_arr n1 then n1 else rand_index curr_arr

(* smart ai *)
let place input letter board =
  let a = Array.map Array.copy board in
  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a - 1 do
      match a.(i).(j) = input with
      | true -> a.(i).(j) <- letter
      | false -> ()
    done
  done;
  a

let rec minimax board depth isMaximizing =
  if set_terminal board "X" then -10
  else if set_terminal board "O" then 10
  else if tie board then 0
  else if isMaximizing then (
    let best_score = ref min_int in
    let remaining = rem_spots board in
    let consider_move x =
      (* place O in empty spot *)
      let board' = place (string_of_int x) "O" board in
      let score = minimax board' (depth + 1) false in
      best_score := max score !best_score
    in
    List.iter consider_move remaining;
    !best_score)
  else
    let best_score = ref max_int in
    let remaining = rem_spots board in
    let consider_move x =
      (* place X in empty spot *)
      let board' = place (string_of_int x) "X" board in
      let score = minimax board' (depth + 1) true in
      best_score := min score !best_score
    in
    List.iter consider_move remaining;
    !best_score

(* given a board [best_move b] returns the position on the board that will lead
   to O's win *)
let best_move board =
  let best_score = ref min_int in
  let move = ref 0 in
  let remaining = rem_spots board in
  let consider_move x =
    let board' = place (string_of_int x) "O" board in
    let score = minimax board' 0 false in
    if score > !best_score then (
      best_score := score;
      move := x)
    else ()
  in
  List.iter consider_move remaining;
  !move
